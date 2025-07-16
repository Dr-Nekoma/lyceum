# Erlang Server

- [Erlang Server](#erlang-server)
  - [Architecture](#architecture)
    - [World Application](#world-application)
    - [Player Application](#player-application)
    - [MNESIA Storage](#mnesia-storage)
    - [Utilities Modules](#utilities-modules)

## Architecture

### World Application

### Player Application

```mermaid
graph TD
    PA[Player Application]

    DS[Dispatcher Supervisor]
    D[Dispatcher]

    PS[Dynamic Player Supervisor]
    P1[Player #1]
    P2[Player #2]
    P3[Player #3]
    PN[...]

    L[Libraries]
    LPSQL[PostgreSQL Library]
    LO[Utility Modules]

    PA --> DS
    PA --> PS
    PA --> L

    PS --> P1
    PS --> P2
    PS --> P3
    PS --> PN

    DS --> D
    D -.->|Player Authentication| PS

    L -.-> LPSQL
    L -.-> LO
    
    classDef umbrellaApp fill:white,stroke:black,stroke-width:3px
    classDef otpApp fill:#f3e5f5,stroke:#4a148c,stroke-width:2px
    classDef supervisor fill:#fff3e0,stroke:#e65100,stroke-width:2px
    classDef dynamicSup fill:#fff8e1,stroke:#ff8f00,stroke-width:2px
    classDef process fill:#e8f5e8,stroke:#2e7d32,stroke-width:1px
    classDef library fill:#fce4ec,stroke:#c2185b,stroke-width:2px,stroke-dasharray: 5 5
    
    class A umbrellaApp
    class PA otpApp
    class DS supervisor
    class PS dynamicSup
    class D,P1,P2,P3,PN process
    class L,LPSQL,LO library
```

### MNESIA Storage

### Utilities Modules
