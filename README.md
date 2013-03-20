semver
======

Implementation of semantic versioning in elisp. See http://semver.org/
for details.

It has functions to parse, format and manipulate version in semver format. All public functions can take either a string or a parsed instance. All functions return a parsed instance, except semver-format which returns a string representation. Example usage:

    (semver-format (semver-inc-minor "1.2.4")) ; "1.3.0"
