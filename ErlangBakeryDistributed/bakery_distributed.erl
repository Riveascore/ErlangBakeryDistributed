-module(bakery_distributed).
-export([manager/2, server/0, create_and_run_manager/3, make_customer/2, create_population/2]).
-import(fib, [fibo/1]).

make_customer(ManagerPid, NumberLeftToSpawn) ->
    Time = crypto:rand_uniform(0, 1000),
    timer:sleep(Time),
    io:fwrite("Customer ~w has entered the bakery ^_^ random nubmer is: ~w~n", [NumberLeftToSpawn, Time]),
		Input = crypto:rand_uniform(1, 50),
    CustomerObject = [{id, NumberLeftToSpawn}, {input, Input}],
    
    ManagerPid ! {customer, CustomerObject}.

create_population(0, ManagerPid) ->
    ManagerPid;
create_population(NumberLeftToSpawn, ManagerPid) ->
    spawn(bakery_distributed, make_customer, [ManagerPid, NumberLeftToSpawn]),
    create_population(NumberLeftToSpawn-1, ManagerPid).

create_and_run_manager(ManagerNode, ListOfHostNodes, NumberOfCustomers) ->
    Servers = lists:map(fun(HostNode) -> spawn(HostNode, bakery_distributed, server, []) end, ListOfHostNodes), 
    ManagerPid = spawn(ManagerNode, bakery_distributed, manager, [Servers, []]),
    create_population(NumberOfCustomers, ManagerPid).

manager(ServerList, []) ->
    receive
	{customer, CustomerObject} ->
	    manager(ServerList, [CustomerObject])
    end;
manager([], CustomerList) ->
    receive
	{customer, AddedCustomer} ->
	    NewCustomerList = CustomerList ++ [AddedCustomer]
    after 0 ->
	    NewCustomerList = CustomerList
    end,

    receive
	{server, FreeServer, CustomerObject} ->
	    [ID, Input, Result] = CustomerObject,
			Output = [element(2,ID), element(2,Input), element(2,Result)],
	    io:fwrite("Customer ~w with ~w dollars, was given ~w brownies!~n", Output),
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

    Server ! {customer, Customer, self()},

    receive
	{server, FreeServer, CustomerObject} ->
			ID = lists:nth(1, CustomerObject),
			Input = lists:nth(2, CustomerObject),
			Result = lists:nth(3, CustomerObject),

	    Output = [element(2, ID), element(2, Input), element(2, Result)],
	    io:fwrite("Customer ~w with ~w dollars, was given ~w brownies!~n", Output),
	    NewerServerList = lists:append(NewServerList, [FreeServer]),
	    NewerServerList = NewServerList ++ [FreeServer],
	    manager(NewerServerList, NewCustomerList)
    after 0 ->
	    manager(NewServerList, NewCustomerList)	    
    end.

server() ->

    receive
	{customer, CustomerObject, ManagerPID} ->
			ID = lists:nth(1, CustomerObject),
			io:fwrite("Customer ~w is being served by server ~w!~n", [ID, self()]),
			Input = lists:nth(2, CustomerObject),
			InputFibNumber = element(2, Input),
	    Result = {result, fib:fibo(InputFibNumber)},
	    ServedCustomer = [ID, Input, Result],
			
	    ManagerPID ! {server, self(), ServedCustomer}
    end,
    server().
