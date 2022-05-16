#ifndef INTRINSICS_STRING_T_H
#define INTRINSICS_STRING_T_H

#include "stdint.h"
#include "stdlib.h"


typedef const uintptr_t ptr_t;
typedef uint16_t teller_t;

typedef struct {
    teller_t size;
    char* chars;
} string_t;


string_t* alloc_string(const teller_t size){
    // add one to size for the initial byte
    string_t* ptr = (string_t*)malloc(sizeof(teller_t) + size);
    ptr->size = size;
    return ptr;
}

string_t* new_string(const teller_t size, const char* cs){
    string_t* str = alloc_string(size);
    for (teller_t i = 0; i < size; i++)
        str->chars[i] = cs[i];
    return str;
}

void free_string(string_t* str){
    free(str);
}

string_t* take_string(const teller_t count, string_t* src){
    if (count > src->size)
        return src;
    else {
        string_t* str = alloc_string(count);
        str->chars = src->chars;
        return str;
    }
}

string_t* drop_string(const teller_t count, string_t* src){
    if (count == src->size)
        return alloc_string(0);
    else if (count > src->size)
        return src;
    else {
        string_t* str = alloc_string(str->size - count);
        str->chars = src->chars + count;
        return str;
    }
}

string_t* concat_string(string_t* str1, string_t* str2){
    // add one to new_size for the initial byte
    string_t* new_str = (string_t*)realloc(str1, str1->size + str2->size + 1);
    for (teller_t j = 0; j < str2->size; j++)
        new_str->chars[str1->size + j] = str2->chars[j];
    free_string(str2);
    return new_str;
}

string_t* ch_to_str(const char ch){
    string_t* str = alloc_string(1);
    str->chars[0] = ch;
    return str;
}
#endif