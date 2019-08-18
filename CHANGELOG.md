Changelog
=========

Version 0.1.2.0
---------------

*August 17, 2019*

<https://github.com/mstksg/hmatrix-vector-sized/releases/tag/v0.1.2.0>

*   Fixed bugs that would occur if ever converting anything that contained an
    hmatrix vector or matrix that was created using `konst`.  This does change
    the API slightly in a potentially breaking way, as some functions now
    require `KnownNat` constraints.

Version 0.1.1.x
---------------

*Nov 13, 2018*

*   <https://github.com/mstksg/hmatrix-vector-sized/releases/tag/v0.1.1.1>

    Fix building on GHC 8.6.

*   <https://github.com/mstksg/hmatrix-vector-sized/releases/tag/v0.1.1.2>

    Fix tests building on GHC 8.6.

*   <https://github.com/mstksg/hmatrix-vector-sized/releases/tag/v0.1.1.3>

    Optimization for matrix converters.

Version 0.1.1.0
---------------

*Feb 11, 2018*

<https://github.com/mstksg/hmatrix-vector-sized/releases/tag/v0.1.1.0>

*   Conversions to and from flattened versions of matrices.

Version 0.1.0.0
---------------

*Feb 10, 2018*

<https://github.com/mstksg/hmatrix-vector-sized/releases/tag/v0.1.0.0>

*   Initial release
