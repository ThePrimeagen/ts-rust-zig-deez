(let ((asdf:*central-registry* (cons #P"./" asdf:*central-registry*)))
  (ql:quickload "deez/executable")
  (asdf:make "deez/executable"))
