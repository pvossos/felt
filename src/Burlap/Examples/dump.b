/* Write an object if nonzero.*/

function writenz (s, x)
    global writes

    if x then
	writes (s, x)
    end
end


/* Write a node. */

function write_node (n)
    global write, writes, null

    writes (n.number, " x=", n.x, " y=", n.y, " z=", n.z)
    writes (" constraint=", n.constraint.name)
    if not null (n.force) then
	writes (" force=", n.force.name)
    end
    write ("")
end


/* Write an element. */

function write_element (e)
    global write, writes

    writes (e.number, " nodes=")
    for i in 1 : e.definition.num_nodes do
	writes (if i == 1 then "[" else "," end, e.nodes (i).number)
    end
    writes ("] material=", e.material.name)
    for i in 1 : e.num_loads do
	writes (if i == 1 then " load=" else " " end, e.loads (i).name)
    end
    write ("")
end


/* Write a material. */

function write_material (m)
    global write, writenz, writes

    writes  (m.name)
    writenz (" E=",	m.E)
    writenz (" Ix=",	m.Ix)
    writenz (" Iy=",	m.Iy)
    writenz (" Iz=",	m.Iz)
    writenz (" A=",	m.A)
    writenz (" J=",	m.J)
    writenz (" G=",	m.G)
    writenz (" t=",	m.t)
    writenz (" rho=",	m.rho)
    writenz (" nu=",	m.nu)
    writenz (" kappa=", m.kappa)
    writenz (" Rk=",	m.Rk)
    writenz (" Rz=",	m.Rm)
    write   ("")
end


/* Write a force. */

function write_force (f)
    global write, writenz, writes

    writes  (f.name)
    writenz (" Fx=", f.Fx)
    writenz (" Fy=", f.Fy)
    writenz (" Fz=", f.Fz)
    writenz (" Mx=", f.Mx)
    writenz (" My=", f.My)
    writenz (" Mz=", f.Mz)
    write   ("")
end


/* Write a constraint. */

function write_constraint (c)
    global write, writes

    writes  (c.name)
    writes  (" Tx=", if c.constraint (1) then "c" else "u" end)
    writes  (" Ty=", if c.constraint (2) then "c" else "u" end)
    writes  (" Tz=", if c.constraint (3) then "c" else "u" end)
    writes  (" Rx=", if c.constraint (4) then "c" else "u" end)
    writes  (" Ry=", if c.constraint (5) then "c" else "u" end)
    writes  (" Rz=", if c.constraint (6) then "c" else "u" end)
    write   ("")
end


/* Write the nodes. */

write ("nodes")
for n in nodes do
    write_node (n)
end


/* Write the elements. */

write ("\nelements")
for e in elements do
    write_element (e)
end


/* Write the materials.  Each material can only be written once. */

write ("\nmaterials")
for e in elements do
    written = 0
    m = e.material

    for i in 1 : e.number - 1 do
	if elements (i).material.name == m.name then
	    written = 1
	    break
	end
    end

    if not written then
	write_material (m)
    end
end


/* Write the forces.  Each force can only be written once. */

write ("\nforces")
for n in nodes do
    written = 0
    f = n.force

    if not null (f) then
	for i in 1 : n.number - 1 do
	    g = nodes (i).force
	    if not null (g) and g.name == f.name then
		written = 1
		break
	    end
	end

	if not written then
	    write_force (f)
	end
    end
end


/* Write the constraints.  Each constraint can only be written once. */

write ("\nconstraints")
for n in nodes do
    written = 0
    c = n.constraint

    for i in 1 : n.number - 1 do
	if nodes (i).constraint.name == c.name then
	    written = 1
	    break
	end
    end

    if not written then
	write_constraint (c)
    end
end


/* Write the loads.  Each load can only be written once. */

write ("\ndistributed loads")

write ("\nend")
