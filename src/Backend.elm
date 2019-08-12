port module Backend exposing (..)
import Types exposing(Song)

import Utils exposing (..)
import Models exposing (Model)
import List exposing (range, map, intersperse)
import String exposing (concat)

-- Existe la funcion findSong que recibe
-- una condicion y una lista de canciones
-- findSong : (Song -> Bool) -> List Song -> Song

-- Existe la funcion tailSafe que recibe
-- una lista de canciones y se queda con la cola
-- si la lista no tiene cola (tiene un solo elemento)
-- se queda con una lista vacia
-- tailSafe : List Song -> List Song

-- Existe idFirst que recibe una lista
-- de canciones y devuelve el id de la primera
-- idFirst : List Song -> String

-- Debería darnos la url de la cancion en base al id
urlById : String -> List Song -> String
urlById id songs = (findSong (esLaMismaId id) songs).url -- use la funcion que esta arriba jajaajaj
-- no se me ocurre como meter la url de cada cancion
--filtro por aquellos que sean igual al id que tengo, pero nose como llamar al id sin mencionarlo
--  (List.filter (id == idCancion songs) songs).url

esLaMismaId : String -> Song -> Bool
esLaMismaId id song = id == song.id

--idCancion : List Song -> List String
--idCancion songs =  []
-- List.map (id) songs -- esta cagada no quiere funcionar, quiero mandarle el id a cada una de las canciones
-- asi me devuelve una lista de ids
-- List.map (id) songs


-- Debería darnos las canciones que tengan ese texto en nombre o artista
filterByName : String -> List Song -> List Song
filterByName text songs = List.filter (nombreOArtista text) songs

-- el switch, tendria que preguntar mañana en persona
nombreOArtista : String -> Song -> Bool
nombreOArtista cancionArtista unaCancion = unaCancion.name == cancionArtista || unaCancion.artist == cancionArtista

-- Recibe un id y tiene que likear/dislikear una cancion
-- switchear song.liked
switchear : Song -> Song
switchear song = { song | liked = not(song.liked)} --Nos devuelve la misma cancion pero con su valor de liked modificado


toggleLike : String -> List Song -> List Song
toggleLike id songs = List.map ((switchearSinoTieneLike<<(findSong (esLaMismaId id) songs)) songs)

switchearSinoTieneLike song = if (song.liked) == True then song
                              else (switchear song)



-- Esta funcion tiene que decir si una cancion tiene
-- nuestro like o no, por ahora funciona mal...
-- hay que arreglarla
isLiked : Song  -> Bool
isLiked song = song.liked -- aparentemente deberia ser asi


-- Recibe una lista de canciones y nos quedamos solo con las que
-- tienen un like
filterLiked : List Song -> List Song
filterLiked songs = List.filter isLiked songs

-- Agrega una cancion a la cola de reproduccion
-- (NO es necesario preocuparse porque este una sola vez)
addSongToQueue : Song -> List Song -> List Song
addSongToQueue song queue = queue++[song]

-- Saca una cancion de la cola
-- (NO es necesario que se elimine una sola vez si esta repetida)
removeSongFromQueue : String -> List Song -> List Song
removeSongFromQueue id queue = List.filter (esLaCancion id) queue

esLaCancion : String -> Song -> Bool
esLaCancion id queue = not(esLaMismaId id queue) -- si no estamos hablado del msimo tema, filtramelo de la lista

-- Hace que se reproduzca la canción que sigue y la saca de la cola
playNextFromQueue : Model -> Model
playNextFromQueue model = model

-------- Funciones Listas --------

-- Esta funcion recibe el modelo y empieza a reproducir la
-- cancion que tenga el id que se pasa...
-- Mirar la función urlById
playSong : Model -> String -> Model
playSong model id = { model | playerUrl = urlById id model.songs, playing = (if id /= "" then Just True else Nothing) }

applyFilters : Model -> List Song
applyFilters model =
  model.songs
    |> filterByName model.filterText
    |> if model.onlyLiked then filterLiked else identity

port togglePlay : Bool -> Cmd msg
port songEnded : (Bool -> msg) -> Sub msg
