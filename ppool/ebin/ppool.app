{application, ppool, [{description, "ppool app from #LYSE"},
                      {vsn, "0.0.1"},
                      {modules, [ppool, ppool_serv, ppool_sup, ppool_supersup, ppool_worker_sup]},
                      {registered, [ppool]},
                      %{env, []},
                      %{maxT, infinity},
                      %{applications, []},
                      {mod, {ppool, []}} ]}.
