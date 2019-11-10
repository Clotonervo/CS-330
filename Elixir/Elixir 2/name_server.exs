defmodule NameServer do
  use GenServer

  # Start Helper Functions (Don't Modify)
  def start_link() do
    GenServer.start_link(__MODULE__, [], [])
  end

  def start() do
    GenServer.start(__MODULE__, [],  [])
  end

  def register(name_server, name) do
    GenServer.call(name_server, {:register, name})
  end

  def register(name_server, name, pid) do
    GenServer.cast(name_server, {:register, name, pid})
  end

  def resolve(name_server, name) do
    GenServer.call(name_server, {:resolve, name})
  end
  #End Helper Functions



  def init(_) do
    #This would be a good place to start a new data structure for keeping pid names
    #Your code here
  end

  def handle_call(first_thing,second_thing,third_thing ) do

    #Change the parameter names appropriately
    #Your code here

  end

  def handle_call(first_thing,second_thing,third_thing) do

    #Change the parameter names appropriately
    #Your code here

  end

  def handle_cast(first_thing,second_thing ) do

    #Change the parameter names appropriately
    #Your code here

  end




  def handle_call(request, from, state) do
    super(request, from, state)
  end

  def handle_cast(request, state) do
    super(request, state)
  end

  def hande_info(_msg, state) do
    {:noreply, state}
  end
end
