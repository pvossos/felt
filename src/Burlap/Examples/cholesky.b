function cholesky (a)
    global rows, sqrt, write

    n = rows (a)

    for k in 1 : n do
	t = sqrt (a (k,k))
	a (k,k) = t

	for j in k + 1 : n do
	    a (j,k) = a (j,k) / t
	end

	for j in k + 1 : n do
	    t = a (j,k)
	    for i in k : n do
		a (i,j) = a (i,j) - t * a (i,k)
	    end
	end
    end

    for j in 2 : n do
	for i in j + 1 : n do
	    a (i,j) = 0
	end
    end

    return a
end

m = [1, 2; 3, 4]
x = m * m'

write (x)
write (chol (x))
write (cholesky (x))
