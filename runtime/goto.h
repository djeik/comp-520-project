/*
File        : goto.h
Description : GoLite runtime header definition
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Type definitions and prototypes for the GoLite runtime.
*/

#ifndef GOTO_H
#define GOTO_H

/* Evil void* -> double cast. This is actually illegal, but thankfully we should
only need it to print things. */
#define AS_DOUBLE (double) (int64_t)

/* Produces a pointer to the given element of a go_array. */
#define POINT_ARRAY(SRC, I) (void*) (((char*) (SRC)->backing) + ((I) * (SRC)->elem_size))

/* Indexes into a go_array (i.e. derefs the result of POINT_ARRAY with the given type */
#define INDEX_ARRAY(SRC, I, TY) *((TY**) (POINT_ARRAY(SRC, I)))


enum GOTYPE {
    UNKNOWN_GT = 0,
    INT1_GT,
    INT2_GT,
    INT4_GT,
    INT8_GT,
    FLOAT_GT,
    STRING_GT, // The only time we care about strings is when we print them.
    ARRAY_GT,
    SLICE_GT,
    STRUCT_GT
};
typedef enum GOTYPE GOTYPE;


/* GoLite managed arrays. Contain the length, element size, inner type (for
copying purposes) and actual data.*/
struct go_array {
    int64_t len, elem_size;
    GOTYPE array_type;
    void* backing;
};
typedef struct go_array go_array;

/* GoLite slices. Basically arrays with an added data point (the capacity, which
can be greater than the length). */
struct go_slice {
    int64_t cap;
    go_array arr_data;
};
typedef struct go_slice go_slice;

/* GoLite structs. We keep track of all the fields for copying purposes. */
struct go_struct {
    int64_t num_fields;
    GOTYPE* types; // Types of the fields
    int64_t* offsets; // Offsets in bytes of each field

    /* An array of the fields. Every field is either represented by a pointer,
    or, for the base types, by its own data. */
    void *fields;
};
typedef struct go_struct go_struct;


/* Runtime functions. See goto.c for documentation. */

void goprint(GOTYPE ty, void* par);

go_array* from_cstr(char* s);

void* index_slice(go_slice* s, int64_t i);
void* index_array(go_array* a, int64_t i);

go_slice* slice_slice(go_slice* s, int64_t low, int64_t high, int64_t bound);
go_slice* slice_array(go_array* a, int64_t low, int64_t high, int64_t bound);

int64_t golen_slice(go_slice* s);
int64_t golen_array(go_array* a);

go_slice* goappend_slice(go_slice* s, void* e);
void assign_into_array(go_array* dst, int64_t n, void* e);

go_array* concat_strings(go_array* a, go_array* b);

int64_t gocopy(go_slice* src, go_slice* dst);

int64_t gocap(go_slice* s);

void panic(char* message);

/* Functions for deep-copying managed objects. */
go_struct* deepcopy_struct(go_struct* src);
go_array* deepcopy_array(go_array* src);
void deepcopy_backing_array(go_array* dst, go_array* src);

void* struct_field(go_struct* s, int64_t i);
size_t storage_size(GOTYPE t);

/* The function for deep-copying slices is gocopy; in other situations, slices
are shallow-copied */
go_slice* shallowcopy_slice(go_slice* src);

/* Functions for creating and initializing new managed items. */
go_array* new_array(int64_t type[]);
go_slice* new_slice(int64_t type[]);
go_struct* new_struct(int64_t type[]);

/* A counter to tell callers how much we advanced in the array of types. This is
only used for structs, but absolutely necessary. It's global because all new_*
functions need to use it. */
int64_t gConsumed = 0;

#endif