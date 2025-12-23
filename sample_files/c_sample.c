/**
 * @file sample.c
 * @brief Comprehensive C language sample demonstrating all syntax features
 * @author Sample Author
 * @version 1.0.0
 */

#ifndef SAMPLE_H
#define SAMPLE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <math.h>
#include <pthread.h>

/* Preprocessor definitions */
#define MAX_BUFFER_SIZE 1024
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define STRINGIFY(x) #x
#define CONCAT(a, b) a ## b
#define DEBUG_LOG(fmt, ...) fprintf(stderr, "[DEBUG] " fmt "\n", ##__VA_ARGS__)

#ifdef __linux__
    #define PLATFORM "Linux"
#elif defined(_WIN32)
    #define PLATFORM "Windows"
#elif defined(__APPLE__)
    #define PLATFORM "macOS"
#else
    #define PLATFORM "Unknown"
#endif

#pragma once
#pragma pack(push, 1)

/* Type definitions */
typedef unsigned char byte_t;
typedef int32_t error_code_t;
typedef void (*callback_fn)(void *data, size_t len);

/* Enumeration types */
typedef enum {
    STATUS_OK = 0,
    STATUS_ERROR = -1,
    STATUS_PENDING = 1,
    STATUS_TIMEOUT = 2
} status_t;

typedef enum color {
    COLOR_RED   = 0xFF0000,
    COLOR_GREEN = 0x00FF00,
    COLOR_BLUE  = 0x0000FF,
    COLOR_WHITE = 0xFFFFFF,
    COLOR_BLACK = 0x000000
} color_t;

/* Structure definitions */
struct point {
    double x;
    double y;
    double z;
};

typedef struct {
    char name[64];
    int age;
    float salary;
    bool is_active;
    struct point location;
} employee_t;

typedef struct node {
    void *data;
    struct node *next;
    struct node *prev;
} node_t;

/* Union types */
typedef union {
    int32_t i;
    float f;
    char bytes[4];
} data_union_t;

/* Bit fields */
struct flags {
    unsigned int is_valid   : 1;
    unsigned int is_ready   : 1;
    unsigned int priority   : 4;
    unsigned int reserved   : 26;
};

#pragma pack(pop)

/* Global variables */
static int global_counter = 0;
volatile sig_atomic_t signal_received = 0;
extern const char *program_name;
static const double PI = 3.14159265358979323846;
static const char * const WEEKDAYS[] = {
    "Monday", "Tuesday", "Wednesday", "Thursday",
    "Friday", "Saturday", "Sunday"
};

/* Function prototypes */
static inline int square(int x);
int factorial(int n);
void *thread_function(void *arg);
int compare_ints(const void *a, const void *b);
char *string_duplicate(const char *src) __attribute__((malloc));
void cleanup_handler(void) __attribute__((destructor));

/* Inline function */
static inline int square(int x) {
    return x * x;
}

/* Recursive function */
int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

/* Function with pointer parameters */
void swap(int *a, int *b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

/* Function returning pointer */
char *string_duplicate(const char *src) {
    if (src == NULL) {
        return NULL;
    }

    size_t len = strlen(src) + 1;
    char *dest = (char *)malloc(len);

    if (dest != NULL) {
        memcpy(dest, src, len);
    }

    return dest;
}

/* Variadic function */
void log_message(const char *format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stdout, format, args);
    va_end(args);
    fputc('\n', stdout);
}

/* Function pointer usage */
int apply_operation(int x, int y, int (*operation)(int, int)) {
    return operation(x, y);
}

int add(int a, int b) { return a + b; }
int subtract(int a, int b) { return a - b; }
int multiply(int a, int b) { return a * b; }

/* Comparison function for qsort */
int compare_ints(const void *a, const void *b) {
    return (*(int *)a - *(int *)b);
}

/* Thread function */
void *thread_function(void *arg) {
    int *value = (int *)arg;
    printf("Thread received value: %d\n", *value);
    *value *= 2;
    return arg;
}

/* Linked list operations */
node_t *create_node(void *data) {
    node_t *node = (node_t *)malloc(sizeof(node_t));
    if (node != NULL) {
        node->data = data;
        node->next = NULL;
        node->prev = NULL;
    }
    return node;
}

void free_list(node_t *head) {
    node_t *current = head;
    while (current != NULL) {
        node_t *next = current->next;
        free(current);
        current = next;
    }
}

/* File operations */
int read_file(const char *filename, char *buffer, size_t size) {
    FILE *fp = fopen(filename, "rb");
    if (fp == NULL) {
        perror("Failed to open file");
        return -1;
    }

    size_t bytes_read = fread(buffer, 1, size - 1, fp);
    buffer[bytes_read] = '\0';

    if (ferror(fp)) {
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return (int)bytes_read;
}

/* Memory operations with error handling */
void *safe_malloc(size_t size) {
    void *ptr = malloc(size);
    if (ptr == NULL && size > 0) {
        fprintf(stderr, "Fatal: Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }
    return ptr;
}

void *safe_realloc(void *ptr, size_t size) {
    void *new_ptr = realloc(ptr, size);
    if (new_ptr == NULL && size > 0) {
        fprintf(stderr, "Fatal: Memory reallocation failed\n");
        free(ptr);
        exit(EXIT_FAILURE);
    }
    return new_ptr;
}

/* Bit manipulation */
uint32_t set_bit(uint32_t value, int bit) {
    return value | (1U << bit);
}

uint32_t clear_bit(uint32_t value, int bit) {
    return value & ~(1U << bit);
}

uint32_t toggle_bit(uint32_t value, int bit) {
    return value ^ (1U << bit);
}

bool test_bit(uint32_t value, int bit) {
    return (value & (1U << bit)) != 0;
}

/* Cleanup handler */
void cleanup_handler(void) {
    printf("Cleanup handler called\n");
}

/* Main function demonstrating various features */
int main(int argc, char *argv[], char *envp[]) {
    /* Variable declarations */
    int i, j, k;
    int array[10] = {0};
    int matrix[3][3] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
    char *str = "Hello, World!";
    char buffer[MAX_BUFFER_SIZE];
    const char *const_ptr = "Constant string";

    /* Numeric literals */
    int decimal = 42;
    int octal = 0755;
    int hex = 0xDEADBEEF;
    int binary = 0b10101010;  /* C23 */
    long long_val = 123456789L;
    unsigned long ulong_val = 123456789UL;
    float float_val = 3.14f;
    double double_val = 3.14159265358979;
    long double ldouble_val = 3.14159265358979L;
    float scientific = 1.23e-4f;

    /* Character literals */
    char ch = 'A';
    char newline = '\n';
    char tab = '\t';
    char backslash = '\\';
    char quote = '\'';
    char null_char = '\0';
    char hex_char = '\x41';
    char octal_char = '\101';

    /* String literals */
    char *multiline = "This is a very long string that "
                      "spans multiple lines in the source "
                      "but is concatenated by the compiler.";

    wchar_t *wide_str = L"Wide string";
    char *utf8_str = u8"UTF-8 string: こんにちは";

    /* Pointer operations */
    int value = 100;
    int *ptr = &value;
    int **ptr_to_ptr = &ptr;
    void *void_ptr = ptr;

    printf("Value: %d, *ptr: %d, **ptr_to_ptr: %d\n",
           value, *ptr, **ptr_to_ptr);

    /* Array and pointer arithmetic */
    int numbers[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int *p = numbers;

    for (i = 0; i < 10; i++) {
        printf("numbers[%d] = %d, *(p + %d) = %d\n",
               i, numbers[i], i, *(p + i));
    }

    /* Sizeof operator */
    printf("sizeof(int) = %zu\n", sizeof(int));
    printf("sizeof(array) = %zu\n", sizeof(array));
    printf("sizeof(employee_t) = %zu\n", sizeof(employee_t));

    /* Control flow - if/else */
    if (argc > 1) {
        printf("Arguments provided: %d\n", argc - 1);
    } else if (argc == 1) {
        printf("No arguments provided\n");
    } else {
        printf("Unexpected argc value\n");
    }

    /* Ternary operator */
    int max_val = (decimal > hex) ? decimal : hex;
    const char *result = (max_val > 100) ? "Large" : "Small";

    /* Switch statement */
    status_t status = STATUS_OK;
    switch (status) {
        case STATUS_OK:
            printf("Status: OK\n");
            break;
        case STATUS_ERROR:
            printf("Status: Error\n");
            break;
        case STATUS_PENDING:
            printf("Status: Pending\n");
            /* fallthrough */
        case STATUS_TIMEOUT:
            printf("Status: Timeout or Pending\n");
            break;
        default:
            printf("Status: Unknown\n");
            break;
    }

    /* For loop */
    for (i = 0; i < 10; i++) {
        array[i] = i * i;
        if (i == 5) continue;
        if (i == 8) break;
        printf("array[%d] = %d\n", i, array[i]);
    }

    /* While loop */
    j = 0;
    while (j < 5) {
        printf("j = %d\n", j);
        j++;
    }

    /* Do-while loop */
    k = 0;
    do {
        printf("k = %d\n", k);
        k++;
    } while (k < 3);

    /* Nested loops with labels */
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 3; j++) {
            printf("matrix[%d][%d] = %d\n", i, j, matrix[i][j]);
        }
    }

    /* Goto statement (use sparingly) */
    int error = 0;
    if (error) {
        goto cleanup;
    }

    /* Structure usage */
    employee_t emp = {
        .name = "John Doe",
        .age = 30,
        .salary = 50000.0f,
        .is_active = true,
        .location = {.x = 1.0, .y = 2.0, .z = 3.0}
    };

    printf("Employee: %s, Age: %d, Salary: %.2f\n",
           emp.name, emp.age, emp.salary);

    /* Union usage */
    data_union_t data;
    data.i = 0x41424344;
    printf("As int: %d, As bytes: %c%c%c%c\n",
           data.i, data.bytes[0], data.bytes[1],
           data.bytes[2], data.bytes[3]);

    /* Function pointer array */
    int (*operations[])(int, int) = {add, subtract, multiply};
    printf("add(5, 3) = %d\n", operations[0](5, 3));
    printf("subtract(5, 3) = %d\n", operations[1](5, 3));
    printf("multiply(5, 3) = %d\n", operations[2](5, 3));

    /* Dynamic memory allocation */
    int *dynamic_array = (int *)malloc(10 * sizeof(int));
    if (dynamic_array == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        return EXIT_FAILURE;
    }

    for (i = 0; i < 10; i++) {
        dynamic_array[i] = i * 10;
    }

    /* Realloc */
    dynamic_array = (int *)realloc(dynamic_array, 20 * sizeof(int));
    if (dynamic_array == NULL) {
        fprintf(stderr, "Memory reallocation failed\n");
        return EXIT_FAILURE;
    }

    /* Calloc */
    int *zeroed_array = (int *)calloc(10, sizeof(int));

    /* qsort usage */
    int unsorted[] = {5, 2, 8, 1, 9, 3, 7, 4, 6, 0};
    qsort(unsorted, 10, sizeof(int), compare_ints);

    /* bsearch usage */
    int key = 5;
    int *found = (int *)bsearch(&key, unsorted, 10, sizeof(int), compare_ints);
    if (found != NULL) {
        printf("Found %d at index %ld\n", *found, found - unsorted);
    }

    /* String operations */
    char dest[100];
    strcpy(dest, "Hello");
    strcat(dest, ", ");
    strcat(dest, "World!");
    printf("Concatenated: %s (length: %zu)\n", dest, strlen(dest));

    /* Memory operations */
    memset(buffer, 0, sizeof(buffer));
    memcpy(buffer, "Test", 4);

    /* Math operations */
    double x = 2.0;
    printf("sqrt(%.1f) = %.4f\n", x, sqrt(x));
    printf("pow(%.1f, 3) = %.4f\n", x, pow(x, 3));
    printf("sin(PI/2) = %.4f\n", sin(PI / 2));
    printf("cos(PI) = %.4f\n", cos(PI));
    printf("log(10) = %.4f\n", log(10));
    printf("exp(1) = %.4f\n", exp(1));

    /* Compound literals (C99) */
    struct point *pt = &(struct point){1.0, 2.0, 3.0};
    printf("Point: (%.1f, %.1f, %.1f)\n", pt->x, pt->y, pt->z);

    /* Designated initializers */
    int sparse[100] = {[0] = 1, [50] = 2, [99] = 3};

    /* Variable length arrays (C99) */
    int vla_size = 5;
    int vla[vla_size];
    for (i = 0; i < vla_size; i++) {
        vla[i] = i;
    }

    /* _Generic selection (C11) */
    #define type_name(x) _Generic((x), \
        int: "int", \
        float: "float", \
        double: "double", \
        default: "unknown")

    printf("Type of decimal: %s\n", type_name(decimal));
    printf("Type of float_val: %s\n", type_name(float_val));

    /* Static assertions (C11) */
    _Static_assert(sizeof(int) >= 4, "int must be at least 4 bytes");

    /* Atomic operations (C11) */
    _Atomic int atomic_counter = 0;
    atomic_counter++;

    /* Thread creation */
    pthread_t thread;
    int thread_arg = 42;

    if (pthread_create(&thread, NULL, thread_function, &thread_arg) != 0) {
        perror("Failed to create thread");
        goto cleanup;
    }

    pthread_join(thread, NULL);
    printf("Thread modified value to: %d\n", thread_arg);

    /* Mutex usage */
    pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
    pthread_mutex_lock(&mutex);
    global_counter++;
    pthread_mutex_unlock(&mutex);
    pthread_mutex_destroy(&mutex);

cleanup:
    /* Resource cleanup */
    free(dynamic_array);
    free(zeroed_array);

    printf("Program completed successfully on %s\n", PLATFORM);

    return EXIT_SUCCESS;
}

#endif /* SAMPLE_H */
