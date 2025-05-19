/* gcc -g -c replace-dm-with-compatible-anon-dm-1-v0.c */
struct user_type
{
  union
  {
    int replaced_member;
    struct
    {
      int member;
    } rh_kabi_hidden_5;
  };
};

void
function(struct user_type * u __attribute__((unused)))
{
}
