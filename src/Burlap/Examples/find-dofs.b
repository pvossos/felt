function find_dofs (shared num_dofs)
    global elements, write, zeros

    dofs = zeros (1, 6)
    flag = zeros (1, 6)

    last_type = null
    for e in elements do
	if last_type != e.definition then
	    last_type = e.definition
	    for i in e.definition.dofs do
		if i then
		    flag (i) = 1
		end
	    end
	end
    end

    num_dofs = 0
    for i in 1 : 6 do
	if flag (i) then
	    num_dofs = num_dofs + 1
	    dofs (i) = num_dofs
	end
    end

    return dofs
end
