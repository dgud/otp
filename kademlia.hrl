%%%-------------------------------------------------------------------
%%% @author Zandra Hird
%%% @copyright (C) 2016, Zandra Hird
%%% @doc
%%%
%%% @end
%%% Created : 2016-09-27 by Zandra Hird
%%%-------------------------------------------------------------------

-define(KEY_SIZE(BIT_SIZE), (1 bsl (BIT_SIZE))).
-define(K, 8).
-define(DISTANCE(A, B), A bxor B).

-record(id, {key::key(), pid::pid()}).
-record(routing_table, {self :: id(),
                        neighbours :: [monitor_ref()],
                        k_buckets :: k_buckets(),
                        neighbour_size = 8 :: non_neg_integer()}).
-record(state,
        {routing_table :: #routing_table{},
         key_bit_sz :: non_neg_integer(),
         unstable :: [non_neg_integer()]}).

-type key() :: non_neg_integer().
-type id() ::  #id{}.
-type monitor_ref() :: {reference(), pid()}.
-type k_bucket() :: {monitor_ref(), [id()]}.
-type k_buckets() :: #{}.

