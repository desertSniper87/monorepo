with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "env";

  # Custom new packages using buildPythonPackage expression
  #_django_rest_passwordreset = with python37Packages; buildPythonPackage rec {
    #name = "django_rest_passwordreset-1.0.0";
    #src = fetchurl {
      #url = "https://nero-mirror.stanford.edu/pypi/web/packages/34/f5/8bf32378a9183bbef9f195d4c4fb2a4ce97b2e56b1f88214434a2036bb64/django-rest-passwordreset-1.0.0.tar.gz";
      #sha256 = "abd27349cd5702d3704f87412a2cd51630b5da95a3ffde01f123e586d7d46b5a";
    #};
    ## Fix broken packaging (package is missing README.rst)
    #prePatch = "touch README.rst";
    ## Define package requirements (without pythonPackages prefix)
    #propagatedBuildInputs = [django_2_2];
  #};

  env = buildEnv { name = name; paths = buildInputs; };
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup; ln -s $env $out
  '';

  buildInputs = [
    postgresql_10
    #zlib
    #zlib.dev
    (python37.buildEnv.override {
      ignoreCollisions = true;
      extraLibs = with python37Packages; [
        # Add pythonPackages without the prefix
        virtualenv
        pip
        django_2_2
        djangorestframework
        djangorestframework-jwt
        psycopg2
        django-cors-headers
        pillow
        #_django_rest_passwordreset
      ];
    })
  ];

  shellHook = ''
    #export SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt
  '';
}
