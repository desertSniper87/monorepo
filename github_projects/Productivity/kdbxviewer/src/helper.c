

#include <stdio.h>     /* for printf */
#include <stdlib.h>    /* for exit */
#include <unistd.h>    /* for getopt */
#include <string.h>

#include <key_tree.h>

#include "helper.h"

const char* empty = "-";

const char* getfield(cx9r_kt_entry* e, char* name) {
    cx9r_kt_field *f = cx9r_kt_entry_get_fields(e);
    while(f != NULL) {
        if (strcmp(cx9r_kt_field_get_name(f), name) == 0) {
            char* v = cx9r_kt_field_get_value(f);
            if (v) return v; else return empty;
        }
        f = cx9r_kt_field_get_next(f);
    }
    return empty;
}

