{-# OPTIONS_GHC -Wno-missing-fields #-}

import Data.List
import Text.Show.Functions

{-
Nombre: Montes, Fabian
Legajo: 143754-9
-}



data Participante = Participante {
    nombreParticipante :: String,
    edad :: Double,
    nivelDeAtractivo :: Double,
    personalidad :: Double,
    inteligencia :: Double
} deriving (Eq, Show)



javiert :: Participante
javiert = Participante {
    nombreParticipante = "javier tulei",
    edad = 52,
    nivelDeAtractivo = 30,
    personalidad = 70,
    inteligencia = 35
}
minimok :: Participante
minimok = Participante {
    nombreParticipante = "minimo kirchner",
    edad = 46,
    nivelDeAtractivo = 0,
    personalidad = 40,
    inteligencia = 50
}


horaciob :: Participante
horaciob = Participante {
    nombreParticipante = "horacio berreta",
    edad = 57,
    nivelDeAtractivo = 10,
    personalidad = 60,
    inteligencia = 50
}


myriamb :: Participante
myriamb = Participante {
    nombreParticipante = "myriam bregwoman",
    edad = 57,
    nivelDeAtractivo = 40,
    personalidad = 40,
    inteligencia = 60
}



data PruebaSemanal = PruebaSemanal {
    nombrePrueba :: String,
    criterioSerSuperado :: CriterioSerSuperado,
    indiceDeExito :: IndiceDeExito
} deriving (Show)

type CriterioSerSuperado = Participante -> Bool
type IndiceDeExito = Participante -> Double


--punto 1

baileDeTikTok :: PruebaSemanal

baileDeTikTok = PruebaSemanal {
    nombrePrueba = "baile de tiktok",
    criterioSerSuperado = criterioSuperarBaileTikTok,
    indiceDeExito = indiceExitoBaileTikTok
}

--se aplica composicion
criterioSuperarBaileTikTok :: CriterioSerSuperado
criterioSuperarBaileTikTok = (>=20) . personalidad

indiceExitoBaileTikTok :: IndiceDeExito
indiceExitoBaileTikTok participante = productoAtractivo participante + personalidad participante

productoAtractivo = (*2) . nivelDeAtractivo



botonRojo :: PruebaSemanal
botonRojo = PruebaSemanal {
    nombrePrueba = "boton rojo",
    criterioSerSuperado = criterioSuperarBotonRojo,
    indiceDeExito = indiceExitoBotonRojo
}
criterioSuperarBotonRojo :: CriterioSerSuperado
criterioSuperarBotonRojo participante = criterioPersonalidad 10 participante && criterioInteligencia 20 participante


criterioPersonalidad personalidadRequerida participante =
    personalidadRequerida == personalidad participante

criterioInteligencia inteligenciaRequerida participante =
    inteligenciaRequerida <= inteligencia participante


indiceExitoBotonRojo :: IndiceDeExito
indiceExitoBotonRojo _ = 100

cuentasRapidas :: PruebaSemanal
cuentasRapidas = PruebaSemanal {
    nombrePrueba = "cuentas rapidas",
    criterioSerSuperado = criterioSuperarCuentasRapidas,
    indiceDeExito = indiceExitoCuentasRapidas
    }

--se ejecuta aplicacion parcial
criterioSuperarCuentasRapidas = criterioInteligencia 40


indiceExitoCuentasRapidas participante = inteligencia participante + personalidad participante + nivelDeAtractivo participante

pruebaSemanalFallida pruebaSemanal participante = PruebaSemanalFinalizada {
    nombrePruebaFinalizada = nombrePrueba pruebaSemanal,
    criterioFinalizado = False,
    indiceDeExitoFinalizado = 0
}

pruebaSemanalSuperada pruebaSemanal participante = PruebaSemanalFinalizada {
    nombrePruebaFinalizada = nombrePrueba pruebaSemanal,
    criterioFinalizado = criterioSuperarPruebaSemanal pruebaSemanal participante,
    indiceDeExitoFinalizado = indiceDeExitoPruebaSemanal pruebaSemanal participante
}

data PruebaSemanalFinalizada = PruebaSemanalFinalizada {
    nombrePruebaFinalizada :: String,
    criterioFinalizado :: Bool,
    indiceDeExitoFinalizado :: Double
} deriving (Show)



type Participantes = [Participante]

participantes :: Participantes
participantes = [javiert, minimok, horaciob, myriamb]


-- punto 2

indiceDeExitoPruebaSemanal :: PruebaSemanal -> IndiceDeExito
indiceDeExitoPruebaSemanal pruebaSemanal participante 
    |nombrePrueba pruebaSemanal == "baile de tiktok" = indiceExitoBaileTikTok participante
    |nombrePrueba pruebaSemanal == "boton rojo" = indiceExitoBotonRojo participante
    |nombrePrueba pruebaSemanal == "cuentas rapidas" = indiceExitoCuentasRapidas participante

criterioSuperarPruebaSemanal :: PruebaSemanal -> CriterioSerSuperado
criterioSuperarPruebaSemanal pruebaSemanal participante 
    |nombrePrueba pruebaSemanal == "baile de tiktok" = criterioSuperarBaileTikTok participante
    |nombrePrueba pruebaSemanal == "boton rojo" = criterioSuperarBotonRojo participante
    |nombrePrueba pruebaSemanal == "cuentas rapidas" = criterioSuperarCuentasRapidas participante

participanteSePrueba :: Participante -> PruebaSemanal -> PruebaSemanalFinalizada
participanteSePrueba participante pruebaSemanal = PruebaSemanalFinalizada {
    nombrePruebaFinalizada = nombrePrueba pruebaSemanal,
    criterioFinalizado = criterioSerSuperado pruebaSemanal participante,
    indiceDeExitoFinalizado = indiceDeExitoPruebaSemanal pruebaSemanal participante
}

participanteQuePrueba :: Participante -> PruebaSemanal -> PruebaSemanalFinalizada
participanteQuePrueba participante pruebaSemanal
    |criterioFinalizado (participanteSePrueba participante pruebaSemanal) = pruebaSemanalSuperada pruebaSemanal participante
    |otherwise = pruebaSemanalFallida pruebaSemanal participante

    {-

participantesQueSuperanPrueba :: [Participante] -> PruebaSemanal -> [Participante]
participantesQueSuperanPrueba participante pruebaSemanal = 
    filter (==True) (criterioFinalizado (participanteQuePrueba participante pruebaSemanal)) participantes pruebaSemanalFinalizada

-}


