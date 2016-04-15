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
*/

#include <stdint.h>

typedef enum {
    UNKNOWN_GT = 0,
    INT_GT,
    FLOAT_GT,
    STRING_GT,
    ARRAY_GT,
    SLICE_GT,
    STRUCT_GT,
} GOTYPE;

/* GoLite managed arrays. Contain the length, element size, inner type (for
copying purposes) and actual data.*/
typedef struct {
    int64_t len, elem_size;
    GOTYPE array_type;
    void* backing;
} go_array;

/* GoLite slices. Basically arrays with an added data point (the capacity, which
can be greater than the length). */
typedef struct {
    int64_t cap;
    go_array arr_data;
} go_slice;

/* GoLite structs. We keep track of all the fields for copying purposes. */
typedef struct {
    int64_t num_fields;
    GOTYPE* types; // Types of the fields
    int64_t* offsets; // Offsets in bytes of each field

    /* An array of the fields. Every field is either represented by a pointer,
    or, for the base types, by its own data. */
    void *fields;
} go_struct;

/* Used by the print, println builtins. */
void goprint(GOTYPE ty, void* par);

/* Converts an ASCIIZ string into a managed golite string. */
go_array* from_cstr(char* s);

/* Index into a slice. */
void* index_slice(go_slice* s, int64_t i);

/* Index into an array. */
void* index_array(go_array* a, int64_t i);

/* Slice into a slice. */
go_slice* slice_slice(go_slice* s, int64_t low, int64_t high, int64_t bound);

/* Slice into an array. */
go_slice* slice_array(go_array* a, int64_t low, int64_t high, int64_t bound);

/* Length of a slice. */
int64_t len_slice(go_slice* s);

/* Length of an array. This is basically used only for strings, since the
length of arrays is statically known. */
int64_t len_array(go_array* a);

/* Appends to a slice. */
go_slice* append_slice(go_slice* s, void* e);

/* Appends to a string. */
go_array* append_string(go_array* a, char c);

/* Copies a slice. Returns the number of elements copied. */
int64_t copy(go_slice* src, go_slice* dst);

/* Queries the capacity of a slice. */
int64_t cap(go_slice* s);