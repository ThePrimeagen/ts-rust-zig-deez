(let ((asdf:*central-registry* (cons #P"./" asdf:*central-registry*)))
  (ql:quickload "deez")
  (ql:quickload "deez/test"))
