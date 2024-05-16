The information about the packages will be stored on a central server and a copy of acquired json files will be saved on the user's machine in a cache. To access and manipulate that information, users are able to use commands. The information is generated directly on the server.

- packages
    - <pkg>
        - <pkg>.json
        - versions
            - <vsn>
                - <vsn>.json
                - modules
                    - <mod>
                        - <mod>.json
                        - types
                            - <type>.json
                        - typeclasses
                            - <typeclass>.json
                        - functions
                            - <func>.json
                        - operators
                            - <op>.json