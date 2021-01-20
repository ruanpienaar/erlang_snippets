-module(map_snippets).

-include_lib("eunit/include/eunit.hrl").

-export([
    map_diff/2
]).

%% @doc
%%  MapA is expected, MapB will be compared.
%%  MapA is what is expected - keys missing will be added to element 2 of
%%  the return value.
%%  element 1 will will show what MapB had extra of different
%%
%% expected - is what was expected
%% extra - is what was provided in comparison
%% missing - is a map of what was expected, but not found in map B
%%
%% @end
map_diff(MapA, MapB) ->
    maps:fold(fun(MapA_Key, MapA_Value, Acc) ->
        #{expected := DiffAcc, missing := MissingAcc, extra := AccMapB} = Acc,
        case maps:get(MapA_Key, AccMapB, map_key_missing) of
            map_key_missing ->
                Acc#{ missing => MissingAcc#{ MapA_Key => MapA_Value } };
            MapB_Value ->
                case MapA_Value =:= MapB_Value of
                    true ->
                        %% remove key from AccMap as key and value matched
                        Acc#{ extra => maps:without([MapA_Key], AccMapB) };
                    false ->
                        %% replace B with what was expected in A.
                        Acc#{ expected => DiffAcc#{ MapA_Key => MapA_Value } }
                end
        end
    end, #{expected => #{}, missing => #{}, extra => MapB }, MapA).

map_diff_test() ->
    %% Simple - both maps same
    ?assertEqual(
        #{
            extra => #{},
            expected => #{},
            missing => #{}
        },
        map_diff(
            #{ key => value },
            #{ key => value }
        )
    ),
    %% MapA (left) has more keys
    ?assertEqual(
        #{
            extra => #{},
            expected => #{},
            missing => #{
                foo => bar
            }
        },
        map_diff(
            #{ key => value, foo => bar },
            #{ key => value }
        )
    ),
    %% MapB (right) has more keys
    ?assertEqual(
        #{
            extra => #{ foo => bar },
            expected => #{},
            missing => #{}
        },
        map_diff(
            #{ key => value },
            #{ key => value, foo => bar }
        )
    ),

    %% Nested maps - both same
    ?assertEqual(
        #{
            extra => #{},
            expected => #{},
            missing => #{}
        },
        map_diff(
            #{ key => #{ something => somewhere } },
            #{ key => #{ something => somewhere } }
        )
    ),
    %% Nested maps - MapA-key (left) has expected value
    ?assertEqual(
        #{
            extra => #{ key => #{ something => somewhere } },
            expected => #{ key => #{ something => sometime } },
            missing => #{}
        },
        map_diff(
            #{ key => #{ something => sometime } },
            #{ key => #{ something => somewhere } }
        )
    ),
    %% Nested maps - MapB-key (right) has expected value
    ?assertEqual(
        #{
            extra => #{ key => #{ something => sometime } },
            expected => #{ key => #{ something => somewhere } },
            missing => #{}
        },
        map_diff(
            #{ key => #{ something => somewhere } },
            #{ key => #{ something => sometime } }
        )
    ),
    %% Nested maps - MapB has missing value
    ?assertEqual(
        #{
            extra => #{},
            expected => #{},
            missing => #{
                key => #{ expect_key => expect_value }
            }
        },
        map_diff(
            #{ key => #{ expect_key => expect_value } },
            #{ }
        )
    ),
    %% Nested maps - MapB has extra value
    ?assertEqual(
        #{
            extra => #{
                key => #{ expect_key => expect_value }
            },
            expected => #{},
            missing => #{}
        },
        map_diff(
            #{  },
            #{ key => #{ expect_key => expect_value } }
        )
    ),

    %% Nested maps - 2 seperate maps.
    ?assertEqual(
        #{
            extra => #{
                what => a,
                weird => kind,
                'of' => map
            },
            expected => #{},
            missing => #{
                key1 => value1,
                key2 => value2
            }
        },
        map_diff(
            #{
                key1 => value1,
                key2 => value2
            },
            #{
                what => a,
                weird => kind,
                'of' => map
            }
        )
    ),

    %% deeply nested expected
    ?assertEqual(
        #{
            extra => #{
                key => #{
                    portion => #{ something => somewhere }
                }
            },
            expected => #{
                key => #{
                    portion => #{ foo => bar }
                }
            },
            missing => #{}
        },
        map_diff(
            #{
                key => #{
                    portion => #{
                        foo => bar
                    }
                },
                key2 => #{
                    sub_key2 => #{
                        sub_sub_key2 => #{
                            test => it
                        }
                    }
                }
            },
            #{
                key => #{
                    portion => #{
                        something => somewhere
                    }
                },
                key2 => #{
                    sub_key2 => #{
                        sub_sub_key2 => #{
                            test => it
                        }
                    }
                }
            }
        )
    ).
