#include <stdio.h>
#include <stdlib.h>
#include <git2.h>
#include <git2/types.h>
#include <git2/status.h>
#include <git2/diff.h>

#include "test_ffi.h"

int print_kek(int a) {
    printf("kek %d \n", a);
    return 0;
}

my_struct* foo(int i) {
    my_struct* s = malloc(sizeof(my_struct));
    (*s).arr[0] = i;
    (*s).arr[1] = i+1;
    (*s).arr[2] = i+2;
    (*s).arr[3] = i+3;
    (*s).arr[4] = i+4;
    (*s).str = "kekpuk";

    return s;
}

void bar(my_struct* ptr) {
    printf("kek %d \n", ptr->arr[0]);
    printf("kek %d \n", ptr->arr[1]);
    printf("kek %d \n", ptr->arr[2]);
    printf("kek %d \n", ptr->arr[3]);
    printf("kek %d \n", ptr->arr[4]);
    printf("%s", ptr->str);
    printf("\nend\n");
}

void test_status_size() {
    printf("sizeof git_status_t: %zu \n", sizeof(git_status_t));
    printf("sizeof git_delta_t: %zu \n", sizeof(git_delta_t));
    printf("sizeof git_diff_file: %zu \n", sizeof(git_diff_file));
    printf("sizeof git_diff_delta: %zu \n", sizeof(git_diff_delta));
    printf("sizeof git_branch_t: %zu \n", sizeof(git_branch_t));
    printf("sizeof int: %zu \n", sizeof(unsigned int));
    printf("sizeof long: %zu \n", sizeof(unsigned long));
}