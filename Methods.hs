module Methods where

plainHuntDoubles :: Method
plainHuntDoubles = Changes 5 UnknownGroup Symmetric [Make [5],
                                                     Make [1],
                                                     Make [5],
                                                     Make [1]]
plainHuntMinor :: Method
plainHuntMinor = Changes 6 UnknownGroup Symmetric [Exchange, Make [1, 6],
                                                   Exchange, Make [1, 6],
                                                   Exchange, Make [1, 6],
                                                   Exchange, Make [1, 6],
                                                   Exchange, Make [1, 6],
                                                   Exchange, Make [1, 6]]

