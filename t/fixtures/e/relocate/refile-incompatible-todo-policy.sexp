; Org 9.8.10 preserves WAIT during refile; the contract rejects incompatible TODO meanings.
(:name "refile-incompatible-todo-policy"
 :divergence :org-preserves-incompatible-todo
 :oracle-fixture "t/fixtures/e/oracle/relocate/refile-incompatible-todo.sexp"
 :contract-result :reject
 :contract-reason :uneditable-structure)
