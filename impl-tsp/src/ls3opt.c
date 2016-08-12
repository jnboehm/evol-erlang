#include <erl_nif.h>
#include <stdio.h>

static ERL_NIF_TERM combinations_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM head, tail = argv[0], res;
    ERL_NIF_TERM a_val, b_val, c_val, comb;
    ERL_NIF_TERM *comb_lis;
    int lis_len, ret;
    int i, a, b, c;
    int* lis;
    unsigned size, jj = 0;
    if (!enif_get_list_length(env, argv[0], &lis_len)) {
        return enif_make_badarg(env);
    }

    lis = enif_alloc(sizeof(unsigned) * lis_len);
    /* printf("lis_len: %d, ", lis_len); */
    for (i = 0; i < lis_len; ++i) {
        enif_get_list_cell(env, tail, &head, &tail);
        enif_get_int(env, head, (&lis[i]));
    }

    // we know that there are only this many options (actually we only
    // need 1/3 of that)
    size = lis_len * (lis_len - 1) * (lis_len - 2) >> 1;
    /* printf("size: %u, ", size); */
    comb_lis = enif_alloc(sizeof(ERL_NIF_TERM) * size);
    for (a = 0; a < lis_len; a++)
        for (b = a + 1; b < lis_len; b++)
            for (c = b + 1; c < lis_len; c++) {
                a_val = enif_make_int(env, lis[a]);
                b_val = enif_make_int(env, lis[b]);
                c_val = enif_make_int(env, lis[c]);
                comb  = enif_make_list3(env, a_val, b_val, c_val);
                comb_lis[jj] = comb;
                jj++;
            }
    /* printf("jj: %d\n", jj); */

    /* free(lis); */
    res = enif_make_int(env, size);
    res = enif_make_list_from_array(env, comb_lis, jj);
    /* free(comb_lis); */
    return res;
}

static ErlNifFunc nif_funcs[] = {
    {"combinations", 1, combinations_nif},
};

ERL_NIF_INIT(optmove3, nif_funcs, NULL, NULL, NULL, NULL)
