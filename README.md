semver
======

Implementation of semantic versioning in elisp. See http://semver.org/
for details.

It has functions to parse, format and manipulate version in semver format. All public functions can take either a string or a parsed instance. All functions return a parsed instance, except semver-format which returns a string representation. Example usage:

    (semver-format (semver-inc-minor "1.2.4")) ; "1.3.0"

Notes
-----

It seems version 1.0.0 of semver doesn't have build numbers and indicates pre-release with a dash and alnums (1.2.3-pre1).

Formats used for a version in js:

    v1.2.3

Formats used for dependency version in js:

    *
    2
    1.x
    0.1.x
    1.0.2-1.2.3 ; this is a full version, not a range!
    ~1.2
    >=0.6
    ~1.2.3
    = 1.2.3
    = 0.1.2-1
    ~0.6.0-1
    ~0.2.2rc
    >=0.5.x
    1.8.1-3

    1.2.3-7-beta (build and tag reversed)

    Operators: * (all version, goes alone), ~ (allow higher patches), >=, =
    Version formats: 1, 1.0, 1.0.3, 1.x, 1.2.x, 1.2.3-2 (build!) 0.2.2rc (tag attached) 2.0.2-7-beta

Node semver accepts the following ranges:

    >1.2.3
    >=1.2.3
    <1.2.3
    <=2.3.4
    1.2.3 - 2.3.4 := >=1.2.3 <=2.3.4
    ~1.2 := >= 1.2.0 <1.3.0
    ~1 := >=1.0.0 <2.0.0
    1.2.x := >=1.2.0 <1.3.0
    1.x := >=1.0.0 <2.0.0
    Join with space '>1.2.3 <2.0.0' means AND
    Join with || '1.2.3 || 1.3.4' means OR
    Couldn't find it, but I guess AND has higher precedence (it marks ends or ranges, while or gives different ranges). Mulitple OR-s makes sence, multiple AND's not (>1.2.3 >2.3.4 <3.4.5)
