(executables
  (names
    %{tests}
  )
  (libraries GT OCanren OCanren.tester)
  (preprocess (action
    (run %{project_root}/camlp5/pp5+gt+plugins+ocanren+logger+dump.exe %{input-file})
    ;(run %{project_root}/camlp5/pp5+gt+plugins+ocanren+logger+o.exe %{input-file})
    ))
  (preprocessor_deps
    (file %{project_root}/camlp5/pp5+gt+plugins+ocanren+logger+dump.exe)
    (file %{project_root}/camlp5/pp5+gt+plugins+ocanren+logger+o.exe))
)
