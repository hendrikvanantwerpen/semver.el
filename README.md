semver
======

Implementation of semver 1 plus some common extensions for elisp. See http://semver.org/ for details.

It has functions to parse, format and manipulate version in semver format. All public functions can take either a string or a parsed instance. All functions return a parsed instance, except semver-format which returns a string representation. Example usage:

    (semver-format (semver-inc-minor "1.2.4")) ; "1.3.0"

Depends on 's.

Version formats
---------------

Versions with the following formats are accepted:

    1.2.3
    1.2.3-4 ; 4 is the build
    1.2.3-beta ; beta is the pre-release

If the part following the first dash is a number (digits only) it is interpreted as a build number. In all other cases are considered as pre-release. This includes the '1.0' in 1.2.3-1.0 for example.

Predicate formats
-----------------

There are quite a few predicate notations out there. This library tries to be compatible with the node-semver package. Accepted formats are:

    1.2.3      ; a literal version
    =1.2.3     ; idem
    >=1.2.3    ; every version bigger in any part than this is accepted.
    >1.2.3     ; 1.2.3 doesn't match, but 1.2.3-1 does, or 1.2.4
    ~1.2.3     ; allows for increases in the patch component.
               ; NB. also matches any 1.2.3-prerelease (this behaviour is copied from node-semver)
    ~1.2       ; equal to ~1.2.0
    ~1         ; this is equal to 1 and does allow variation in minor as well.
    1 - 2      ; accepts any version from the lowest accepted version of the first part
               ; until the highest accepted version of the second part
    1.3 - 1.4  ; E.g. anything >= 1.3 and <= 1.5 will match.
    1.x, 1.x.x ; same as '1'
    1.2.x      ; same as 1.2
    1.2 || 2.1 ; any of the above separated by || indicate one of them

Notes in semver 2
-----------------

Combining semver 1 + common extensions and semver 2 can be tricky. The case with the buildnumber (1.2.3-2) might be seen as a prerelease according to semver 2. Also sorting is a problem, because in semver 1 the pre-release is sorted lexically, where in semver 2 it splits by dots and handles digit only identifiers as number.

