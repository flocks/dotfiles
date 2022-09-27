
(require 'edbi)

(defvar ft-sql--databases '("dbi:Pg:dbname=gate_minivault;host=localhost"
			    "dbi:Pg:dbname=hsm_driver;host=localhost"))

(defun ft-psql-connect ()
  (interactive)
  (let* ((db (completing-read "Database: " ft-sql--databases))
	 (conn (edbi:start)))
    (edbi:connect conn (list db "vault" "vault"))
    (edbi:dbview-open conn)))


(ft-psql-connect)
