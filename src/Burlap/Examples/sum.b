# compute the sum of the elements of a scalar, vector, or matrix

function sum (x)
    sum = 0
    for v in x do
	for w in v do
	    sum = sum + w
	end
    end
    return sum
end
