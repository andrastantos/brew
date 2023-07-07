Family compatibility
====================

The Brew specification doesn't strive for complete cross-family binary compatibility. It leaves many details to the implementation, such as MMU, CSRs and I/O. As a consequence, processors implementing the Brew architecture in general are not going to be compatible to one another on the system software level. The goal is that - combined with the right system software - these processors could still execute the same, unmodified user applications.