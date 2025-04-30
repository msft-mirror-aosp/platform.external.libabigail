#include <stdint.h>

struct s1
{
  uint64_t m0;
  union
  {
    struct
    {
      uint32_t e0;
      uint32_t e1;
    };
    uint8_t pad[32];
  } m1;
};

struct s1 global_var;
