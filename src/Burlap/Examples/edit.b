# edit the specified file

function edit (file, editor)
    if (null? (editor)) then editor = "vi" end
    return system (concat (editor, concat (" ", file)))
end
