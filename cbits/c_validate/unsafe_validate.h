//
// Created by Kirill Golubev on 11.01.2022.
//

#ifndef C_VALIDATE_UNSAFE_VALIDATE_H
#define C_VALIDATE_UNSAFE_VALIDATE_H
extern "C"{
    void start_python();
    bool unsafe_validate(const char* object, const char* scheme);
    char* unsafe_generate(const char* scheme);
    void dispose_generated_object(char *);
    void end_python();
};

#endif //C_VALIDATE_UNSAFE_VALIDATE_H
