# iex -r myElixirFile.exs

defmodule Elixir_Intro do

#------------------------------- fib(n)
    def fib(n) do
        case n do
            0 ->
                0
            1 ->
                1
            _ ->
                fibHelper(1, 1, n-2)
        end
    end

    def fibHelper(current_fib, next_fib, n) do
        case n do
            0 ->
                next_fib
            _ ->
                fibHelper(next_fib, current_fib + next_fib, n-1)
        end
    end

#------------------------------- area(shape, shape_info)
    def area(shape, shape_info) do
        case shape do
            :rectangle ->
                elem(shape_info, 0) * elem(shape_info, 1)
            :square ->
                shape_info * shape_info
            :circle ->
                (shape_info * shape_info) * :math.pi
            :triangle ->
                elem(shape_info, 0) * elem(shape_info, 1) / 2
        end
    end

#------------------------------- sqrList(nums)
    def sqrList(nums) do
        sqrListHelper(nums)
    end

    def sqrListHelper([head | tail]) do
        [head * head | sqrListHelper(tail)]
    end

    def sqrListHelper([]) do
        []
    end

#------------------------------- calcTotals(inventory)
#******************************************************************************************** Check this with TAs
# Elixir_Intro.calcTotals([{:apple, 3, 2}, {:pear, 5, 1}, {:pumpkin, 1, 10}])

    def calcTotals(inventory) do
        calcTotalsHelper(inventory)
    end

    def calcTotalsHelper([head | tail]) do
        [{elem(head, 0), elem(head, 1) * elem(head,2)} | calcTotalsHelper(tail)]
    end

    def calcTotalsHelper([]) do
        []
    end



#------------------------------- map(function,vals)
    def map(function, vals) do
        mapHelper(function, vals)
    end

    def mapHelper(function, [head | tail]) do
        [ function.(head) | mapHelper(function, tail)]
    end

    def mapHelper(_function, []) do
        []
    end


#------------------------------- quickSortServer()
		def quickSortServer() do
			receive do
				{message, pid} -> send(pid, {quickSort(message), self()})
			end
			quickSortServer()
		end

		def quickSort([]) do
				[]
		end

		def quickSort(list) do
			pivot = :rand.uniform(0..(length(list) - 1))
		end

end

#------------------------------- Client
defmodule Client do
    def callServer(pid,nums) do
        send(pid, {nums, self()})
	listen()
    end

    def listen do
        receive do
	    {sorted, pid} -> sorted
	end
    end
end
