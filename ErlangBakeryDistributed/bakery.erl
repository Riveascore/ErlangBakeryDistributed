-module(bakery).
-export([manager/2, serve/3, create_and_run_manager/2, make_customer/2, create_population/2]).
-import(fib, [fibo/1]).

make_customer(ManagerPid, NumberLeftToSpawn) ->
    Time = random:uniform(10000),
    timer:sleep(Time),
    io:fwrite("Customer ~w has entered the bakery ^_^ random nubmer is: ~w~n", [NumberLeftToSpawn, Time]),
    ManagerPid ! {customer, NumberLeftToSpawn}.


create_population(0, ManagerPid) ->
    true;
create_population(NumberLeftToSpawn, ManagerPid) ->
    spawn(bakery, make_customer, [ManagerPid, NumberLeftToSpawn]),
    create_population(NumberLeftToSpawn-1, ManagerPid).


create_and_run_manager(NumberOfServers, NumberOfCustomers) ->
    Servers = lists:seq(1,NumberOfServers),
    
    ManagerPid = spawn(bakery, manager, [Servers, []]),
    create_population(NumberOfCustomers, ManagerPid).

manager(ServerList, []) ->
    receive
	{customer, NumberLeftToSpawn} ->
	    manager(ServerList, [NumberLeftToSpawn])
    end;
manager([], CustomerList) ->
    receive
	{customer, AddedCustomer} ->
	    NewCustomerList = CustomerList ++ [AddedCustomer]
    after 0 ->
	    NewCustomerList = CustomerList
    end,

    receive
	{server, FreeServer} ->
	    NewServerList = [FreeServer],
	    manager(NewServerList, NewCustomerList)
    end;
manager(ServerList, CustomerList) ->
    receive
	{customer, AddedCustomer} ->
	    NextCustomerList = CustomerList ++ [AddedCustomer]
    after 0 ->
	    NextCustomerList = CustomerList
    end,

    [Server|NewServerList] = ServerList,
    [Customer|NewCustomerList] = NextCustomerList,
    io:fwrite("Customer ~w is being served by server ~w!~n", [Customer, Server]),
    spawn(bakery, serve, [Server, Customer, self()]),
    receive
	{server, FreeServer} ->
	    NewerServerList = lists:append(NewServerList, [FreeServer]),
	    NewerServerList = NewServerList ++ [FreeServer],
	    manager(NewerServerList, NewCustomerList)
    after 0 ->
	    manager(NewServerList, NewCustomerList)	    
    end.

serve(Server, Customer, ManagerPid) ->
    
    %% random:uniform(20)
    %% Input = random:uniform(20),
    Input = 30,
    Result = fib:fibo(Input),
    io:fwrite("Customer ~w was given ~w brownies!~n", [Customer, Result]),
    ManagerPid ! {server, Server}.
