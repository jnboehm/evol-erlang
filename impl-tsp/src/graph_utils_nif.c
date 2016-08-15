#include <erl_nif.h>

static ERL_NIF_TERM get_weight_el_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM head, tail = argv[0], res;
    const ERL_NIF_TERM *tup_arr;
    int v1, v1tmp, v2, v2tmp, tup_size;

    if(!enif_get_int(env, argv[1], &v1))
        return enif_make_badarg(env);
    if(!enif_get_int(env, argv[2], &v2))
        return enif_make_badarg(env);

    while(enif_get_list_cell(env, tail, &head, &tail)) {
        enif_get_tuple(env, head, &tup_size, &tup_arr);
        enif_get_int(env, tup_arr[1], &v1tmp);
        enif_get_int(env, tup_arr[2], &v2tmp);
        if(v1 == v1tmp && v2 == v2tmp)
            return tup_arr[3];
    }
    return enif_make_atom(env, "undef");
}

static ErlNifFunc nif_funcs[] = {
    {"get_weight_el", 3, get_weight_el_nif},
};

ERL_NIF_INIT(graph_utils, nif_funcs, NULL, NULL, NULL, NULL)
