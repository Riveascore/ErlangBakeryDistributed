-module(bakery_distributed).
-export([manager/2, server/0, create_and_run_manager/3, make_customer/2, create_population/2]).
-import(fib, [fibo/1]).
%bakery_distributed:create_and_run_manager(MANAGERHERE, [SERVERSHERE], 30).

make_customer(ManagerPid, NumberLeftToSpawn) ->
    io:fwrite("making customer ~w~n", [NumberLeftToSpawn]),
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
	    manager(ServerList, [CustomerObject]);
	{server, FreeServer, CustomerServedObject} ->
	    [ID,Input,Result] = CustomerServedObject,
	    Output = [element(2,ID), element(2,Input), element(2,Result)],
	    io:fwrite("M1 Customer ~w with ~w dollars, was given ~w brownies!~n", Output),
	    NewServerList = ServerList ++ [FreeServer],
	    manager(NewServerList, [])
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
	    Output = [element(2, ID), element(2, Input), element(2, Result)],
	    io:fwrite("M2 Customer ~w with ~w dollars, was given ~w brownies!~n", Output),
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

    receive
	{server, FreeServer, CustomerObject} ->
	    [ID, Input, Result] = CustomerObject,
	    Output = [element(2, ID), element(2, Input), element(2, Result)],
	    io:fwrite("M3 Customer ~w with ~w dollars, was given ~w brownies!~n", Output),
	    NewServerList = ServerList ++ [FreeServer]
    after 0 ->
	    NewServerList = ServerList    
    end,

    [Server|NewerServerList] = NewServerList,
    [Customer|NewCustomerList] = NextCustomerList,
    [CustomerID,_] = Customer,
    io:fwrite("Customer ~w is being served by server ~w!~n", [element(2,CustomerID), node(Server)]),
    Server ! {customer, Customer, self()},
    manager(NewerServerList, NewCustomerList).

server() ->

    receive
	{customer, CustomerObject, ManagerPID} ->	    
       
	    [ID, Input] = CustomerObject,
	    io:fwrite("Customer ~w, is here on server ~w~n", [element(2, ID),node()]),
	    Result = {result, fib:fibo(element(2,Input))},
	    ServedCustomer = [ID, Input, Result],
	    ManagerPID ! {server, self(), ServedCustomer}
    end,
    server().
