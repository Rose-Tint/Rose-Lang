#ifndef INTRINSICS_STRING_T_H
#define INTRINSICS_STRING_T_H
#include "stdint.h"
#include "stdlib.h"


typedef const uintptr_t ptr_t;

typedef struct {
    uint8_t size;
    char* chars;
} string_t;


string_t* alloc_string(const uint8_t size, const char* cs){
    // add one to size for the initial byte
    string_t* ptr = (string_t*)malloc(sizeof(string_t));
    ptr->size = size;
    ptr->chars = (char*)calloc(size + 1, 1);
    for (uint8_t i = 0; i < size; i++)
        *(ptr->chars + i) = *(cs + i);
    return ptr;
}

void free_string(string_t* str){
    free(str->chars);
    free(str);
    return;
}

string_t* string_concat(string_t* str1, string_t* str2){
    // add one to new_size for the initial byte
    string_t* new_str = (string_t*)realloc(str1, str1->size + str2->size + 1);
    for (uint8_t j = 0; j < str2->size; j++)
        new_str->chars[str1->size + j] = str2->chars[j];
    free_string(str2);
    return new_str;
}

string_t* chtostr(const char ch){
    return alloc_string(1, &ch);
}


#endif