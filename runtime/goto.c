/*
File        : goto.c
Description : GoLite runtime
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Specifies the GoLite runtime. All managed objects (arrays, strings, slices and
structs) are defined here, as well as functions for operating on them.

Built-in functions are also declared. Because GoLite builtins are sometimes
polymorphic, multiple versions of those may appear here, to deal properly with
each case.

In places where polymorphism can occur between base and complex types, a void*
is a pointer for complex types, but base types represent themselves. This means
that this void* is cast as a int64_t, float64_t or other base types.

Throughout, GoLite's float64 is represented as a double. This is somewhat
problematic, because the C standard makes no guarantees on what a double actually
is, while the Go spec determines that it follows the IEEE-754 format.
*/

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "goto.h"

extern void gocode_main(void);
extern void init(void);

/* Used by the print, println builtins. */
void goprint(GOTYPE ty, void* par) {
    switch (ty) {
        case INT1_GT:
        case INT2_GT:
        case INT4_GT:
        case INT8_GT:
            printf("%" PRId64, (int64_t) par);
            break;

        case FLOAT_GT:
            printf("%f", AS_DOUBLE par);
            break;

        case STRING_GT:
            if (!par)
                gopanic("goprint: null string argument");

            printf("%s", (char*) ((go_array*) par)->backing);
            break;

        case UNKNOWN_GT: gopanic("Cannot print unknown types");
        case ARRAY_GT: gopanic("Cannot print array types");
        case SLICE_GT: gopanic("Cannot print slice types");
        case STRUCT_GT: gopanic("Cannot print struct types");
        default: gopanic("Unhandled goprint type");
    }
}

/* Converts an ASCIIZ string into a managed golite string. */
go_array* from_cstr(char* s) {
    if (!s)
        gopanic("from_cstr: null argument");

    go_array* a = malloc(sizeof(go_array));
    if (!a)
        gopanic("from_cstr: out of memory");

    a->len = strlen(s);
    a->elem_size = sizeof(char);
    a->array_type = INT1_GT;
    a->backing = s;

    return a;
}

/* Index into a slice. A pointer to the data is returned.
A run-time panic occurs if the index is out of bounds. */
void* index_slice(go_slice* s, int64_t i) {
    if (!s)
        gopanic("index_slice: null argument");

    return index_array(&(s->arr_data), i);
}

/* Index into an array. A pointer to the data is returned.
A run-time gopanic occurs if the index is out of bounds. */
void* index_array(go_array* a, int64_t i) {
    if (!a)
        gopanic("index_array: null argument");

    if (i < 0 || i >= a->len)
        gopanic("index out of bounds");

    return POINT_ARRAY(a, i);
}

/* Slice into a slice. */
go_slice* slice_slice(go_slice* s, int64_t low, int64_t high, int64_t bound) {
    return slice_array(&(s->arr_data), low, high, bound);
}

/* Slice into an array. */
go_slice* slice_array(go_array* a, int64_t low, int64_t high, int64_t bound) {
    if(!a)
        gopanic("slice_array: null argument");

    if (0 > low || low > high || high > bound || bound > a->len)
        gopanic("slice out of bounds");

    go_slice* sli = malloc(sizeof(go_slice));
    if (!sli)
        gopanic("slice_array: could not allocate go_slice");

    sli->cap = bound - low;
    sli->arr_data.len = high - low;
    sli->arr_data.elem_size = a->elem_size;
    sli->arr_data.array_type = a->array_type;
    sli->arr_data.backing = a->backing;

    return sli;
}

/* Length of a slice. */
int64_t golen_slice(go_slice* s) {
    if (!s)
        gopanic("len_slice: null argument");

    return golen_array(&(s->arr_data));
}

/* Length of an array. This is directly used only by strings, since the length
of arrays is statically known. */
int64_t golen_array(go_array* a) {
    if (!a)
        gopanic("len_array: null argument");

    return a->len;
}

/* Appends to a slice. */
go_slice* goappend_slice(go_slice* s, void* e) {
    if(!s)
        gopanic("append_slice: null first argument");

    /* Go spec: "If the capacity of s is not large enough to fit the additional
    values, append allocates a new, sufficiently large underlying array" */
    if (s->arr_data.len >= s->cap) {
        go_slice* nsli = malloc(sizeof(go_slice));

        /* The spec says "sufficiently large", but doesn't say anything about
        extra elements! In the interest of better performance, we grow the slice
        by a factor of 2. In fact, this is also what golang does. */
        int64_t ncap = (int64_t) (s->cap * 2);

        // Little check to prevent problems when the capacity was 0.
        nsli->cap = ncap != s->cap ? ncap : ncap + 1;

        // Use the same underlying array data...
        nsli->arr_data = s->arr_data;
        // ... except with a new backing array
        nsli->arr_data.backing = malloc(nsli->cap * nsli->arr_data.elem_size);

        if (s->cap > 0)
            deepcopy_backing_array(&(nsli->arr_data), &(s->arr_data));

        // TODO: newize all those extra elements we added...

        s = nsli;
    }

    assign_into_array(&(s->arr_data), s->arr_data.len, e);
    s->arr_data.len++;
    return s;
}

void assign_into_array(go_array* dst, int64_t n, void* e) {
    switch (dst->array_type) {
        case UNKNOWN_GT: gopanic("goappend_slice: cannot append unknown type");

        case INT1_GT:
        case INT2_GT:
        case INT4_GT:
        case INT8_GT:
        case FLOAT_GT:
            memcpy(((char*) dst->backing) + n, e, dst->elem_size);
            break;

        case STRING_GT:
        case ARRAY_GT:
            INDEX_ARRAY(dst, dst->len, go_array) = deepcopy_array((go_array*) e);
            break;

        case STRUCT_GT:
            INDEX_ARRAY(dst, dst->len, go_struct) = deepcopy_struct((go_struct*) e);
            break;

        case SLICE_GT:
            // Slices are ONLY deep-copied when the copy() builtin is called.
            INDEX_ARRAY(dst, dst->len, go_slice) = shallowcopy_slice((go_slice*) e);
            break;
    }
}

/* Implements the + operator for strings. */
go_array* concat_strings(go_array* a, go_array* b) {
    if(!a)
        gopanic("concat_strings: null left argument");

    if(!b)
        gopanic("concat_strings: null right argument");

    go_array* dst = malloc(sizeof(go_array));
    if (!dst)
        gopanic("concat_strings: could not allocate go_array");

    dst->len = a->len + b->len;
    dst->elem_size = sizeof(char);
    dst->array_type = INT1_GT;
    dst->backing = malloc(sizeof(char) * (dst->len + 1));
    memcpy(dst->backing, a->backing, a->len);
    memcpy((void*) (((char*) dst->backing) + a->len), b->backing, b->len);
    ((char*) dst->backing)[dst->len] = 0;

    return dst;
}

/* Copies a slice. Returns the number of elements copied. */
int64_t gocopy(go_slice* dst, go_slice* src) {
    if(!src)
        gopanic("gocopy: null source argument");

    if(!dst)
        gopanic("gocopy: null destination argument");

    int64_t len = src->arr_data.len;
    if (len > dst->arr_data.len)
        len = dst->arr_data.len;

    deepcopy_backing_array(&(dst->arr_data), &(src->arr_data));

    return len;
}

/* Queries the capacity of a slice. */
int64_t gocap(go_slice* s) {
    if (!s)
        gopanic("cap: null argument");

    return s->cap;
}

/* Run-time panic (a.k.a. exit) */
void gopanic(char* message) {
    printf("Runtime gopanic: %s\n", message);
    exit(1);
}

/* Make a slice of the given type. This is the one spot where we need to do deep
initialization of slices. */
go_slice* gomake(int64_t type[]) {
    int64_t len = type[1], cap = type[2];
    if (type[0] == UNKNOWN_GT)
        gopanic("gomake: cannot have inner type be unknown.");

    if (len > cap)
        gopanic("gomake: len must be less than or equal to cap");

    go_slice* s = malloc(sizeof(go_slice));
    s->cap = cap;
    s->arr_data.len = len;
    s->arr_data.array_type = type[0];
    s->arr_data.elem_size = storage_size(s->arr_data.array_type);

    s->arr_data.backing = malloc(s->cap * s->arr_data.elem_size);

    gConsumed = 0;
    new_deep_backing_array(&(s->arr_data), type + 3);
    gConsumed = 0;

    return s;
}

/* Deep-copies an array. The array metadata is copied. The backing buffer is
deep-copied according to the type of the array. */
go_array* deepcopy_array(go_array* src) {
    if (!src)
        gopanic("deepcopy_array: null source");

    go_array* dst = malloc(sizeof(go_array));
    if (!dst)
        gopanic("deepcopy_array: could not allocate new go_array");

    dst->len = src->len;
    dst->elem_size = src->elem_size;
    dst->array_type = src->array_type;
    dst->backing = malloc(src->len * src->elem_size);

    deepcopy_backing_array(dst, src);

    return dst;
}

/* Deep-copies the backing array of a go_array. This is used for deep-copying
go_arrays, gocopying go_slices, and allocating new go_slices when append reaches
capacity. */
void deepcopy_backing_array(go_array* dst, go_array* src) {
    int64_t i;
    switch (dst->array_type) {
        case UNKNOWN_GT: gopanic("deepcopy_array: cannot copy unknown type");

        case INT1_GT:
        case INT2_GT:
        case INT4_GT:
        case INT8_GT:
        case FLOAT_GT:
            memcpy(dst->backing, src->backing, src->len * src->elem_size);
            break;

        case STRING_GT:
        case ARRAY_GT:
            for (i = 0; i < src->len; i++)
                INDEX_ARRAY(dst, i, go_array) =
                    deepcopy_array(INDEX_ARRAY(src, i, go_array));
            break;

        case STRUCT_GT:
            for (i = 0; i < src->len; i++)
                INDEX_ARRAY(dst, i, go_struct) =
                    deepcopy_struct(INDEX_ARRAY(src, i, go_struct));
            break;

        case SLICE_GT:
            for (i = 0; i < src->len; i++)
                // Slices are ONLY deep-copied when the copy() builtin is called.
                INDEX_ARRAY(dst, i, go_slice) =
                    shallowcopy_slice(INDEX_ARRAY(src, i, go_slice));
            break;
    }
}

/* Deep-copies a struct. The array metadata is copied. The backing buffer is
deep-copied according to the type of each struct field. */
go_struct* deepcopy_struct(go_struct* src) {
    if (!src)
        gopanic("deepcopy_struct: null source");

    go_struct* dst = malloc(sizeof(go_struct));
    if (!dst)
        gopanic("deepcopy_struct: could not allocate new go_struct");

    dst->num_fields = src->num_fields;
    memcpy(dst->types, src->types, src->num_fields);
    memcpy(dst->offsets, src->offsets, src->num_fields);
    int64_t i;
    void *sfield, *dfield;
    for (i = 0; i < src->num_fields; i++) {
        sfield = struct_field(src, i);
        dfield = struct_field(dst, i);
        switch (src->types[i]) {
            case UNKNOWN_GT: gopanic("deepcopy_struct: cannot copy unknown type");

            case INT1_GT:
            case INT2_GT:
            case INT4_GT:
            case INT8_GT:
            case FLOAT_GT:
                memcpy(dfield, sfield, storage_size(src->types[i]));
                break;

            case STRING_GT:
            case ARRAY_GT:
                *((go_array**) dfield) = deepcopy_array(*(go_array**) sfield);
                break;

            case STRUCT_GT:
                *((go_struct**) dfield) = deepcopy_struct(*(go_struct**) sfield);
                break;

            case SLICE_GT:
                /* Member slices are always shallow-copied. */
                *((go_slice**) dfield) = shallowcopy_slice(*(go_slice**) sfield);
                break;
        }
    }

    return dst;
}

/* Obtains a pointer to the offset for the given field in the struct. */
void* struct_field(go_struct* s, int64_t i) {
    return (void*) ((char*) (s->fields) + s->offsets[i]);
}

/* Returns the storage size of a given type. */
size_t storage_size(GOTYPE t) {
    switch (t) {
        case UNKNOWN_GT: gopanic("unknown type has no storage size");
        case INT1_GT: return sizeof(char);
        case INT2_GT: return sizeof(int16_t);
        case INT4_GT: return sizeof(int32_t);
        case INT8_GT: return sizeof(int64_t);
        case FLOAT_GT: return sizeof(double);

        case STRING_GT:
        case ARRAY_GT:
        case SLICE_GT:
        case STRUCT_GT:
            return sizeof(void*);
    }

    // We'll never get here, but this silences a werror.
    return 0;
}

/* Shallow-copies a slice. The metadata is copied, but the backing array pointed
to is the same in the destination (i.e. only a pointer copy is done).

This occurs in assignments. */
go_slice* shallowcopy_slice(go_slice* src) {
    if (!src)
        gopanic("shallowcopy_slice: null source");

    go_slice* dst = malloc(sizeof(go_slice));
    if (!dst)
        gopanic("shallowcopy_slice: could not allocate new go_slice");

    dst->cap = src->cap;
    dst->arr_data = src->arr_data;

    return dst;
}

/* The functions below create new managed items, and initialize them to their
default value. The type parameter of the functions is understood as follows:

    * For arrays, the top element is considered as the length. The second-to-top
      is considered as the inner type. If this type is not basic, the inner
      elements are recursively created. If it is basic, the backing array is
      zeroed.

    * The top element is considered as the inner type. No recursion occurs on
      slices because new slices have no backing array.

    * For structs, the top element is considered as the number of fields ( = n).
      For each of those, advance through the array, interpreting as much as
      possible as one type, recursively creating managed items as necessary.

    * To create a new string, create a zero-length character array (i.e. call
      new_array with [0, INT1_GT]).

    For example, the following type:
        struct {
            [10]int;
            float64;
            [][]bool;
        }
    is represented as the following list:
    [STRUCT_GT, 3, ARRAY_GT, 10, INT8_GT, FLOAT_GT, SLICE_GT, SLICE_GT, INT1_GT]

    This is extremely ghetto, but means that we don't have to do complicated
    stuff to call these functions from our assembly.
*/

/* Creates and zero-initializes an array */
go_array* new_array(int64_t type[]) {
    int consumed_was_0 = !gConsumed;

    go_array* a = malloc(sizeof(go_array));
    a->len = type[0];
    a->array_type = (GOTYPE) type[1];
    a->elem_size = storage_size(a->array_type);
    a->backing = malloc(a->len * a->elem_size);

    new_deep_backing_array(a, type + 2);

    if (consumed_was_0)
        gConsumed = 0;
    else
        gConsumed += 2;

    return a;
}

/* Creates and zero-initializes a slice */
go_slice* new_slice(int64_t type[]) {
    int consumed_was_0 = !gConsumed;

    go_slice* s = malloc(sizeof(go_slice));
    s->cap = s->arr_data.len = 0;
    s->arr_data.array_type = type[0];
    s->arr_data.elem_size = storage_size(s->arr_data.array_type);

    /* This is safe:
        - Slicing a slice merely copies the pointer, not its data.
        - Shallow-copying also only copies the pointer
        - Appending to a slice creates a new backing array if the length and
          capacity of the slice are equal, which is the case here.
        - Copying from slice to slice copies the minimum length of both. It will
          be 0 in this case, so the backing array will never be accessed.
    */
    s->arr_data.backing = NULL;

    if (consumed_was_0)
        gConsumed = 0;
    else
        gConsumed += 1;

    return s;
}

void new_deep_backing_array(go_array* a, int64_t type[]) {
    int64_t i;
    switch (a->array_type) {
        case UNKNOWN_GT: gopanic("new_deep_backing_array: cannot allocate unknown type");
        case INT1_GT:
        case INT2_GT:
        case INT4_GT:
        case INT8_GT:
        case FLOAT_GT:
            memset(a->backing, 0, a->len * a->elem_size);
            break;

        case STRING_GT:
        case ARRAY_GT:
            for (i = 0; i < a->len; i++)
                INDEX_ARRAY(a, i, go_array) = new_array(type);
            break;

        case SLICE_GT:
            for (i = 0; i < a->len; i++)
                INDEX_ARRAY(a, i, go_slice) = new_slice(type);
            break;

        case STRUCT_GT:
            for (i = 0; i < a->len; i++)
                INDEX_ARRAY(a, i, go_struct) = new_struct(type);
            break;
    }
}

/* Recursively creates and initializes a struct. */
go_struct* new_struct(int64_t type[]) {
    int consumed_was_0 = !gConsumed;

    /* By monitoring gConsumed around recursive calls, we figure out how those
    calls have progressed in the type array. This variable tracks this.*/
    int64_t inner_consumption = 1;
    go_struct* s = malloc(sizeof(go_struct));
    s->num_fields = type[0];
    s->types = malloc(sizeof(GOTYPE) * s->num_fields);
    s->offsets = malloc(sizeof(int64_t) * s->num_fields);

    int64_t i = 0;
    int64_t old_consumed;
    int64_t num_basic_fields = 0; // Number of basic fields encountered in this struct.
    void* dfield;
    for (; i < s->num_fields; i++) {
        s->types[i] = type[inner_consumption];
        s->offsets[i] = (i > 0 ? s->offsets[i - 1] : 0) + storage_size(s->types[i]);
        dfield = struct_field(s, i);
        switch (type[inner_consumption]) {
            case INT1_GT:
            case INT2_GT:
            case INT4_GT:
            case INT8_GT:
            case FLOAT_GT:
                memset(dfield, 0, storage_size(s->types[i]));
                inner_consumption++;
                num_basic_fields++;
                break;

            case STRING_GT:
            case ARRAY_GT:
                old_consumed = gConsumed;
                *((go_array**) dfield) = new_array(type + inner_consumption);
                inner_consumption += gConsumed - old_consumed;
                break;

            case STRUCT_GT:
                old_consumed = gConsumed;
                *((go_struct**) dfield) = new_struct(type + inner_consumption);
                inner_consumption += gConsumed - old_consumed;
                break;

            case SLICE_GT:
                old_consumed = gConsumed;
                *((go_slice**) dfield) = new_slice(type + inner_consumption);
                inner_consumption += gConsumed - old_consumed;
                break;
        }
    }

    if (consumed_was_0)
        gConsumed = 0;
    else
        // Stuff from the recursive calls was already counted in gConsumed.
        // We only need to add the number of fields (1) and all the basic fields.
        gConsumed += 1 + num_basic_fields;

    return s;
}

int main(int argc, char **argv) {
    init();
    gocode_main();
    return 0;
}
