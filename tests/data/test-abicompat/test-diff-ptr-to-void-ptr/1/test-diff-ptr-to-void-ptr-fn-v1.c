typedef void (*PTR_TO_FN)(...);

void
foo(int a __attribute__((unused)),
    char b __attribute__((unused)),
    PTR_TO_FN c __attribute__((unused)))
{
}
