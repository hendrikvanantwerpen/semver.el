(sort (list "1.2.2"
            "1.2.3"
            "1.2.3rc2"
            "1.2.3-rc1"
            "1.2.3-rc2"
            "1.2.3-2.1"
            "1.2.3-1-rc2"
            "1.2.3-2"
            "1.2.3-2-1"
            "1.2.3-2-a"
            "1.2.3-2-2.1"
            "1.2.3-2-a.1"
            "1.2.3-2-rc1"
            "1.2.4"
            "1.3.0"
            "2.0.1-rc2") 'semver<)

(semver-major "1.2.3") ; 1

(semver-minor "1.2.3") ; 2

(semver-patch "1.2.3") ; 3

(semver-inc-major "1.2.3") ; 2.0.0

(semver-inc-minor "1.2.3") ; 1.3.0

(semver-inc-patch "1.2.3") ; 1.2.4

(semver-set-major 2 "1.2.3") ; 2.2.3

(semver-set-minor 3 "1.2.3") ; 1.3.3

(semver-set-patch 4 "1.2.3") ; 1.2.4

(semver-build "1.2.3") ; nil

(semver-build "1.2.3-1") ; 1

(semver-build "1.2.3-beta") ; nil

(semver-prerelease "1.2.3") ; nil

(semver-prerelease "1.2.3-1") ; nil

(semver-prerelease "1.2.3-beta") ; beta

(semver-prerelease "1.2.3-1-beta") ; beta

(semver-initial-p "0.9.0") ; t

(semver-initial-p "1.0.0") ; nil

(semver-public-p "0.9.0") ; nil

(semver-public-p "1.0.0") ; t

(semver-format (semver-parse "1.2.3")) ; 1.2.3

(semver-format (semver-parse "1.2.3-1")) ; 1.2.3-1

(semver-format (semver-parse "1.2.3-rc")) ; 1.2.3-rc

(semver< "1.2.3-1" "1.2.3-2") ; t

(semver< "1.2.3-1" "1.2.3-1") ; nil

(semver< "1.2.3-beta" "1.2.3-1") ; t

(semver< "1.2.3-beta" "1.2.3-rc") ; t

(semver< "1.2.3-1-beta" "1.2.3-beta") ; nil

(semver-satisfies-p "*" "1.2.3") ; t

(semver-satisfies-p "1" "1.2.3") ; t

(semver-satisfies-p "1.2" "1.2.3") ; t

(semver-satisfies-p "1.3" "1.2.3") ; nil

(semver-satisfies-p "1.2 - 1.3" "1.3.3") ; t

(semver-satisfies-p "1.2 - 1.3" "1.2.3") ; t

(semver-satisfies-p "1.3 - 2 || ~1.2.3" "1.2.3-alpha") ; t

(semver-satisfies-p "~1.2" "1.2.4") ; t

(semver-satisfies-p "1.3 - 2 || ~ 1.2.3" "1.3.0-rc1") ; t

(semver-satisfies-p "1.3 - 2 || ~ 1.2.3" "2.3.4") ; t

(semver-satisfies-p ">=1.2.3" "1.2.3") ; t

(semver-satisfies-p ">=1.2.3" "1.2.3-rc") ; nil

(semver-satisfies-p ">1.2.3" "1.2.3") ; nil

(semver-satisfies-p ">1.2.3" "1.2.3-1") ; t

(semver-satisfies-p "~1.2" "1.2.3-rc") ; t

(semver-satisfies-p "~1" "1.2.3") ; t

(semver-satisfies-p "~1.2" "1.1.0") ; nil

