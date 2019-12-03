typedef struct MyStruct {
    double d;
    char c;
    int i;
} MyStruct;


typedef enum {
	KEK_FST = 0,
	KEK_SND        = (1u << 0),
	KEK_THRD   = (1u << 1),
	KEK_FRTH    = (1u << 2),
} kek_t;