-- ===========================================================
-- Main.hs
-- ===========================================================
-- Módulo principal del juego “Haskell Tank Tournament”.
-- Se encarga de gestionar:
--   - El flujo general del juego (menús, configuración, combate)
--   - El ciclo principal de Gloss (renderizado, eventos, actualización)
--   - La inicialización del estado global (GameWorld)
-- ===========================================================

{-# LANGUAGE RecordWildCards #-}

module Main where

-- -----------------------------------------------------------
-- IMPORTS
-- -----------------------------------------------------------
-- Gloss: motor gráfico funcional usado para renderizar y manejar eventos.
import Graphics.Gloss hiding (Polygon, arc)
import Graphics.Gloss.Interface.Pure.Game hiding (Polygon, text)
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss (text)

-- Módulos del proyecto
import qualified Geometry as G
import Entities
import Physics
import CollisionSAT
import GameState
import qualified AI as AI
import Memory

-- Librerías estándar
import Data.Maybe (mapMaybe)
import Data.List (find, findIndex)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (Left, Right)

-- ===========================================================
-- 1) FASES, CONFIGURACIÓN Y ESTADO GLOBAL
-- ===========================================================
-- Esta sección define los tipos que describen el estado global del juego,
-- incluyendo el menú, la pantalla de configuración y la partida activa.
-- ===========================================================

-- | Fases principales del flujo del juego.
data GamePhase
  = MainMenu     -- Menú principal (pantalla inicial)
  | ConfigScreen -- Pantalla de configuración de bots
  | Playing      -- Partida en curso
  deriving (Eq, Show)

-- | Configuración individual de cada bot.
--   Actualmente solo contiene el tipo de IA, pero puede ampliarse.
newtype BotConfig = BotConfig { botAI :: AIType }
  deriving (Show)

-- | Configuración global del torneo:
--   cuántos robots habrá y qué IA usa cada uno.
data GameConfig = GameConfig
  { numRobots  :: Int          -- ^ Número de robots activos
  , botConfigs :: [BotConfig]  -- ^ Lista de configuraciones individuales
  } deriving (Show)

-- | Estado completo del mundo en un momento dado.
--   Contiene la fase del juego, el estado de simulación,
--   la configuración activa y un contador de IDs para proyectiles.
data GameWorld = GameWorld
  { phase     :: GamePhase    -- ^ Fase actual (menú, configuración o partida)
  , gameState :: GameState    -- ^ Estado del juego (robots, balas, explosiones)
  , config    :: GameConfig   -- ^ Configuración activa de la partida
  , nextId    :: Int          -- ^ Siguiente ID disponible para proyectiles o robots
  }

-- ===========================================================
-- 2) CONSTANTES Y ESTADOS INICIALES
-- ===========================================================
-- Define constantes globales (dimensiones, colores, FPS) y
-- los estados iniciales que se usan al arrancar el juego.
-- ===========================================================

-- ---------- Dimensiones de pantalla ----------
screenWidth, screenHeight :: Int
screenWidth  = 800
screenHeight = 600

-- ---------- Configuración de la ventana ----------
window :: Display
window = InWindow "Haskell Tank Tournament"
                  (screenWidth, screenHeight)
                  (100, 100)

-- ---------- Color de fondo ----------
backgroundColor :: Color
backgroundColor = makeColor 0.1 0.1 0.1 1.0

-- ---------- Frecuencia de refresco ----------
fps :: Int
fps = 60

-- ---------- Tamaños de entidades ----------
-- Cuerpo del robot
robotWidth, robotHeight :: Float
robotWidth  = 40
robotHeight = 30

-- Torreta del robot
turretWidth, turretHeight :: Float
turretWidth  = 25
turretHeight = 8

-- Proyectiles
projectileWidth, projectileHeight, projectileSpeed :: Float
projectileWidth  = 18
projectileHeight = 6
projectileSpeed  = 300

-- Duración de vida del proyectil (segundos)
projectileLifetime :: Float
projectileLifetime = 2.5

-- ---------- Estado de juego vacío ----------
-- Se usa como base antes de crear los robots o el mapa.
emptyGameState :: GameState
emptyGameState = GameState
  { robots      = []  -- Sin robots todavía
  , projectiles = []  -- Sin balas activas
  , explosions  = []  -- Sin explosiones
  , time        = 0   -- Tiempo inicial
  , gameMap     = GameMap
      (fromIntegral screenWidth)
      (fromIntegral screenHeight)
      []  -- Lista de obstáculos vacía
  }

-- ---------- Configuración por defecto ----------
-- Dos bots tipo Hunter listos para pelear.
defaultConfig :: GameConfig
defaultConfig = GameConfig
  { numRobots  = 2
  , botConfigs = replicate 2 (BotConfig Hunter)
  }

-- ---------- Estado inicial del mundo ----------
-- Es el punto de partida al ejecutar el juego o tras reiniciar.
initialWorld :: GameWorld
initialWorld = GameWorld
  { phase     = MainMenu        -- Empieza en el menú principal
  , gameState = emptyGameState  -- Sin robots ni balas
  , config    = defaultConfig   -- Configuración inicial (2 bots)
  , nextId    = 1               -- Primer ID disponible
  }

-- ===========================================================
-- 3) FUNCIÓN PRINCIPAL (MAIN)
-- ===========================================================
-- Gloss se encarga de manejar el ciclo de juego.
-- Aquí se definen las funciones que controlan:
--   - renderizado (render)
--   - manejo de eventos (handleEvent)
--   - actualización del estado (updateGame)
-- ===========================================================

main :: IO ()
main = play
  window          -- Ventana principal
  backgroundColor -- Color de fondo
  fps             -- Fotogramas por segundo
  initialWorld    -- Estado inicial del juego
  render          -- Función para dibujar
  handleEvent     -- Función para manejar eventos (teclado/ratón)
  updateGame      -- Función para actualizar el estado (cada tick)

-- ===========================================================
-- 4) DISPATCHERS POR FASE
-- ===========================================================
-- Este bloque actúa como “central de control”.
-- Dependiendo de la fase actual del juego (menú, configuración o partida),
-- redirige las funciones de renderizado, eventos y actualización
-- al módulo o bloque correspondiente.
-- ===========================================================

-- | Renderiza la escena según la fase actual del juego.
--   Devuelve una 'Picture' que Gloss mostrará en pantalla.
render :: GameWorld -> Picture
render GameWorld{..} = case phase of
  MainMenu     -> drawMainMenu               -- Pantalla principal
  ConfigScreen -> drawConfigScreen config    -- Pantalla de configuración
  Playing      -> drawGame gameState         -- Juego activo (combate)

-- | Gestiona los eventos de entrada (ratón, teclado) según la fase.
--   La tecla 'r' reinicia el juego a su estado inicial.
handleEvent :: Event -> GameWorld -> GameWorld
handleEvent ev w@GameWorld{..}
  -- Si se pulsa la tecla 'r', se reinicia completamente el juego.
  | EventKey (Char 'r') Down _ _ <- ev = initialWorld
  | otherwise = case phase of
      MainMenu     -> handleMenuEvents ev w      -- Eventos del menú principal
      ConfigScreen -> handleConfigEvents ev w    -- Eventos en pantalla de configuración
      Playing      -> handleGameEvents ev w      -- Eventos en partida (aún vacío)

-- | Actualiza el estado del juego cada frame.
--   Solo ejecuta la lógica cuando se está jugando.
updateGame :: Float -> GameWorld -> GameWorld
updateGame dt w@GameWorld{..} =
  case phase of
    Playing -> runGameLogic dt w   -- Avanza la simulación
    _       -> w                   -- En menús, no cambia nada


-- ===========================================================
-- 5) MENÚS
-- ===========================================================
-- Aquí se definen las interfaces visuales del menú principal y
-- de la pantalla de configuración, junto con la lógica para
-- detectar clics en botones y reaccionar a ellos.
-- ===========================================================

-- ---------- Helpers de interfaz ----------
-- | Dibuja un botón rectangular con texto centrado.
drawButton :: String -> Float -> Float -> Float -> Float -> Picture
drawButton txt x y w h = pictures
  [ translate x y $ color (greyN 0.2) $ rectangleSolid w h
  , translate x y $ color white       $ rectangleWire  w h
  , translate (x - w/2 + 15) (y - 10)
      $ scale 0.2 0.2
      $ color white
      $ Gloss.text txt
  ]

-- | Determina si un clic del ratón está dentro de un rectángulo dado.
isClickInBox :: G.Point -> Float -> Float -> Float -> Float -> Bool
isClickInBox (mx, my) x y w h =
  mx >= x - w/2 && mx <= x + w/2 &&
  my >= y - h/2 && my <= y + h/2


-- ---------- Menú principal ----------
-- | Dibuja la pantalla inicial con el título y el botón “Start Game”.
drawMainMenu :: Picture
drawMainMenu = pictures
  [ translate (-250)  50
      $ scale 0.5 0.5
      $ color white
      $ Gloss.text "Haskell Tank Tournament"

  , drawButton "Start Game" 0 (-50) 200 40

  , translate (-250) (-200)
      $ scale 0.15 0.15
      $ color white
      $ Gloss.text "Press R at any time to return here."
  ]

-- | Gestiona los clics en el menú principal.
--   Si el jugador pulsa sobre “Start Game”, cambia a la pantalla de configuración.
handleMenuEvents :: Event -> GameWorld -> GameWorld
handleMenuEvents (EventKey (MouseButton LeftButton) Down _ (mx,my)) w =
  if isClickInBox (mx,my) 0 (-50) 200 40
     then w { phase = ConfigScreen }
     else w
handleMenuEvents _ w = w


-- ---------- Pantalla de configuración ----------
-- | Dibuja la interfaz donde se puede ajustar el número de bots
--   y elegir el tipo de IA de cada uno antes de iniciar el torneo.
drawConfigScreen :: GameConfig -> Picture
drawConfigScreen GameConfig{..} = pictures $
  [ translate (-200) 250 $ scale 0.3 0.3 $ color white $ Gloss.text "Configure Tournament"
  , translate (-200) 180 $ scale 0.2 0.2 $ color white $ Gloss.text "Number of Bots:"
  , drawButton "-"   (-20) 170 40 40
  , translate 40 180 $ scale 0.2 0.2 $ color white $ Gloss.text (show numRobots)
  , drawButton "+"    100  170 40 40
  ]
  ++ concatMap drawEntry (zip [0..] botConfigs)
  ++ [ drawButton "Launch Tournament" 0 (-250) 300 50 ]
  where
    -- Dibuja cada línea de configuración individual (uno por bot).
    drawEntry (i, BotConfig{..}) =
      let y = 100 - fromIntegral i * 80
      in [ translate (-250) y
             $ scale 0.2 0.2
             $ color white
             $ Gloss.text ("Bot " ++ show (i+1) ++ ":")
         , drawButton (show botAI) 0 (y - 10) 200 40
         ]

-- | Gestiona los clics dentro de la pantalla de configuración.
--   Permite:
--     - Aumentar o reducir el número de bots
--     - Cambiar el tipo de IA de cada uno
--     - Lanzar la partida
handleConfigEvents :: Event -> GameWorld -> GameWorld
handleConfigEvents (EventKey (MouseButton LeftButton) Down _ (mx,my)) w@GameWorld{..} =
  let GameConfig{..} = config

      -- (+) o (-) número de robots (entre 2 y 4)
      newNum =
        if      isClickInBox (mx,my) (-20) 170 40 40 then max 2 (numRobots - 1)
        else if isClickInBox (mx,my)  100  170 40 40 then min 4 (numRobots + 1)
        else numRobots

      -- Ajusta la lista de configuraciones si cambia el número de bots
      baseCfgs
        | newNum > numRobots = take newNum (botConfigs ++ repeat (BotConfig Hunter))
        | newNum < numRobots = take newNum botConfigs
        | otherwise          = botConfigs

      -- Detecta si el clic se hizo sobre una de las cajas de IA
      aiRects = [ (0, 100 - fromIntegral i * 80 - 10, 200, 40)
                | i <- [0 .. length baseCfgs - 1]
                ]
      clickedIdx = findIndex (\(x,y,w',h') -> isClickInBox (mx,my) x y w' h') aiRects

      -- Si se ha clicado un tipo de IA, se alterna (Hunter ↔ Evasive)
      updatedCfgs =
        case clickedIdx of
          Nothing  -> baseCfgs
          Just idx ->
            let (pre, BotConfig ai : post) = splitAt idx baseCfgs
            in pre ++ [BotConfig (cycleAI ai)] ++ post

      newCfg = config { numRobots = newNum, botConfigs = updatedCfgs }

  -- Si se pulsa el botón "Launch Tournament", se inicia la partida.
  in if isClickInBox (mx,my) 0 (-250) 300 50
     then startGameFromConfig newCfg
     else w { config = newCfg }
handleConfigEvents _ w = w

-- | Cambia el tipo de IA entre Hunter y Evasive.
cycleAI :: AIType -> AIType
cycleAI Hunter  = Evasive
cycleAI Evasive = Hunter


-- ===========================================================
-- 6) INICIALIZACIÓN DE PARTIDA Y LÓGICA PRINCIPAL
-- ===========================================================
-- Aquí comienza la simulación real del combate.
-- Se crean los robots según la configuración elegida
-- y se define el bucle que actualizará su comportamiento.
-- ===========================================================

-- | Crea un nuevo 'GameWorld' a partir de la configuración seleccionada.
--   Inicializa el 'GameState' con los robots y cambia a la fase Playing.
startGameFromConfig :: GameConfig -> GameWorld
startGameFromConfig conf =
  let newGS = createInitialGameStateFromConfig conf
  in initialWorld
       { phase     = Playing
       , gameState = newGS
       , config    = conf
       , nextId    = numRobots conf + 1
       }

-- | Construye el 'GameState' inicial de la partida
--   con los robots colocados en posiciones predeterminadas.
createInitialGameStateFromConfig :: GameConfig -> GameState
createInitialGameStateFromConfig GameConfig{..} =
  let positions = take numRobots robotStartPositions
      rs = zipWith3 (\(p,a) (BotConfig ai) i -> createRobot i p a ai)
                    positions botConfigs [1..]
  in emptyGameState { robots = rs }

-- | Posiciones y orientaciones iniciales de los robots en el mapa.
--   (soportan hasta 4 robots)
robotStartPositions :: [(G.Point, G.Angle)]
robotStartPositions =
  [ ((-200, 200),    -pi/4)
  , (( 200,-200),     3*pi/4)
  , ((-200,-200),     pi/4)
  , (( 200, 200),    -3*pi/4)
  ]

-- | Crea un robot completamente inicializado en la posición indicada.
createRobot :: Int -> G.Point -> G.Angle -> AIType -> Robot
createRobot i pos dir aiType =
  let base = GameObject
        { objId   = i
        , objPos  = pos
        , objDir  = dir
        , objVel  = (0,0)
        , objShape= createRectanglePolygon pos robotWidth robotHeight dir
        , objType = RobotType
        }
      turret = RobotTurret
        { turretDir       = dir
        , turretCooldown  = 0
        , turretMaxCooldown = 1.0
        }
  in Robot
      { robotBase      = base
      , robotHealth    = 100
      , robotMaxHealth = 100
      , robotRange     = 400
      , robotTurret    = turret
      , robotMemory    = emptyMemory
      , robotAIType    = aiType
      }

-- | Bucle principal de actualización cuando la partida está activa.
--   Se ejecuta en cada frame mientras queden al menos dos robots vivos.
runGameLogic :: Float -> GameWorld -> GameWorld
runGameLogic dt w@GameWorld{..} =
  let gs = gameState
  in if countActiveRobots (robots gs) <= 1
        then w  -- Si solo queda un robot, el juego se detiene.
        else
          let (actions, gs0) = getAIActions gs              -- 1️⃣ Obtener acciones de IA
              (gs1, newId)   = applyAllActions nextId dt actions gs0 -- 2️⃣ Aplicarlas
              gs2            = updatePhysics dt gs1         -- 3️⃣ Actualizar físicas
              gs3            = handleCollisions gs2         -- 4️⃣ Gestionar colisiones
          in w { gameState = gs3, nextId = newId }

-- | Manejador de eventos dentro de la partida (actualmente vacío).
handleGameEvents :: Event -> GameWorld -> GameWorld
handleGameEvents _ w = w

-- ===========================================================
-- 7) IA: OBTENER ACCIONES Y APLICARLAS
-- ===========================================================
-- Esta sección conecta el sistema de IA con el estado del juego:
--  - Obtiene las decisiones de cada robot (movimiento, disparo, etc.)
--  - Aplica esas acciones sobre el GameState.
-- ===========================================================

-- | Recoge las acciones de todos los robots vivos.
--   Devuelve una lista con sus IDs y acciones asociadas.
getAIActions :: GameState -> ([(Int, [AI.BotAction])], GameState)
getAIActions gs =
  let alive   = filter (\r -> robotHealth r > 0) (robots gs)
      -- Ejecuta las IAs de cada robot (Hunter o Evasive) mediante IO
      actions = unsafePerformIO $ mapM (getActionsForRobot gs) alive
  in (actions, gs)

-- | Ejecuta el “cerebro” de un robot concreto según su tipo de IA.
getActionsForRobot :: GameState -> Robot -> IO (Int, [AI.BotAction])
getActionsForRobot gs r = do
  let brain = case robotAIType r of
                Hunter  -> AI.hunterBot
                Evasive -> AI.evasiveBot
  acts <- brain gs r
  pure (objId (robotBase r), acts)

-- | Aplica todas las acciones de todos los robots al GameState.
--   Cada robot puede moverse, girar, disparar o actualizar su memoria.
--   Devuelve el nuevo estado del juego y el siguiente ID disponible.
applyAllActions :: Int -> Float -> [(Int, [AI.BotAction])] -> GameState -> (GameState, Int)
applyAllActions startId dt allActs gs =
  let step (currGs, currId) (rid, acts) =
        case findById rid (robots currGs) of
          Nothing -> (currGs, currId) -- Si el robot no existe, no hace nada
          Just r  ->
            -- Aplica todas las acciones de este robot una por una
            let (r', mProj, nextId') = foldl (applyAction dt) (r, Nothing, currId) acts
                rs' = replaceRobot r' (robots currGs)
                ps' = projectiles currGs ++ mapMaybe id [mProj] -- Agrega proyectil si disparó
            in (currGs { robots = rs', projectiles = ps' }, nextId')
  in foldl step (gs, startId) allActs

-- | Aplica una acción individual a un robot.
--   Algunas acciones devuelven un nuevo proyectil (como Shoot).
applyAction :: Float -> (Robot, Maybe Projectile, Int) -> AI.BotAction -> (Robot, Maybe Projectile, Int)
applyAction dt (r, mProj, currId) action = case action of

  -- Movimiento lineal
  AI.MoveSpeed speed ->
    let newVel = updateVelocity speed (objDir (robotBase r))
    in (r { robotBase = (robotBase r) { objVel = newVel } }, mProj, currId)

  -- Rotación del chasis
  AI.Rotate radians ->
    let maxTurnSpeed = 2.5
        capped = signum radians * min (abs radians) (maxTurnSpeed * dt)
        base   = robotBase r
    in (r { robotBase = base { objDir = objDir base + capped } }, mProj, currId)

  -- Rotación de la torreta
  AI.RotateTurret radians ->
    let t = robotTurret r
    in (r { robotTurret = t { turretDir = turretDir t + radians } }, mProj, currId)

  -- Apuntar torreta a una posición específica
  AI.Aim targetPos ->
    let t = robotTurret r
        selfPos = objPos (robotBase r)
        ang = G.angleToTarget selfPos targetPos
    in (r { robotTurret = t { turretDir = ang } }, mProj, currId)

  -- Disparo de proyectil (si la torreta no está en cooldown)
  AI.Shoot ->
    let t = robotTurret r
    in if turretCooldown t <= 0
       then
         let base = robotBase r
             ang  = turretDir t
             (offX, offY) = (turretWidth * cos ang, turretWidth * sin ang)
             startPos = G.addVec (objPos base) (offX, offY)
             proj = Projectile
               { projBase = GameObject
                   { objId   = currId
                   , objPos  = startPos
                   , objDir  = ang
                   , objVel  = updateVelocity projectileSpeed ang
                   , objShape= createRectanglePolygon startPos projectileWidth projectileHeight ang
                   , objType = ProjectileType
                   }
               , projOwner = objId base
               , projDamage = 10
               , projType   = Bullet
               , projLifetime = projectileLifetime
               , projMaxLifetime = projectileLifetime
               }
         in ( r { robotTurret = t { turretCooldown = turretMaxCooldown t } }
            , Just proj
            , currId + 1
            )
       else (r, Nothing, currId)

  -- Guarda un valor en la memoria del robot
  AI.SetMemory k v ->
    (r { robotMemory = set k v (robotMemory r) }, mProj, currId)

  -- No hace nada
  AI.Idle -> (r, mProj, currId)


-- ===========================================================
-- 8) FÍSICA Y COLISIONES
-- ===========================================================
-- Aquí se actualiza el movimiento de objetos, la vida útil de
-- proyectiles y explosiones, y se gestionan las colisiones entre robots y balas.
-- ===========================================================

-- | Incrementa el tiempo total del juego.
updateGameTime :: Float -> GameState -> GameState
updateGameTime dt gs = gs { time = time gs + dt }

-- | Actualiza la física de todos los objetos del juego.
--   - Mueve robots y proyectiles
--   - Reduce cooldowns
--   - Elimina proyectiles y explosiones expiradas
updatePhysics :: Float -> GameState -> GameState
updatePhysics dt gs =
  let movedR  = map (updateRobotPosition dt) (robots gs)
      movedP  = map (updateProjectilePosition dt) (projectiles gs)
      coolR   = map (updateRobotCooldown dt) movedR
      aliveP  = mapMaybe (updateProjectileLifetime dt) movedP
      aliveE  = mapMaybe (updateExplosionLifetime dt) (explosions gs)
  in updateGameTime dt gs { robots = coolR, projectiles = aliveP, explosions = aliveE }

-- | Mueve un robot según su velocidad y limita su posición dentro de la pantalla.
updateRobotPosition :: Float -> Robot -> Robot
updateRobotPosition dt r =
  let base0 = robotBase r
      base1 = updatePosition base0 dt
      clamped = clampPosition (objPos base1)  -- evita que salga de la pantalla
      base2 = base1 { objPos = clamped }
      shape = createRectanglePolygon (objPos base2) robotWidth robotHeight (objDir base2)
  in r { robotBase = base2 { objShape = shape } }

-- | Actualiza posición y forma del proyectil.
updateProjectilePosition :: Float -> Projectile -> Projectile
updateProjectilePosition dt p =
  let b1 = updatePosition (projBase p) dt
      shape = createRectanglePolygon (objPos b1) projectileWidth projectileHeight (objDir b1)
  in p { projBase = b1 { objShape = shape } }

-- | Reduce el cooldown de disparo de la torreta.
updateRobotCooldown :: Float -> Robot -> Robot
updateRobotCooldown dt r =
  let t = robotTurret r
  in r { robotTurret = t { turretCooldown = max 0 (turretCooldown t - dt) } }

-- | Disminuye la vida restante de un proyectil, eliminándolo si caduca.
updateProjectileLifetime :: Float -> Projectile -> Maybe Projectile
updateProjectileLifetime dt p =
  let t = projLifetime p - dt
  in if t <= 0 then Nothing else Just p { projLifetime = t }

-- | Disminuye la duración de una explosión visual.
updateExplosionLifetime :: Float -> Explosion -> Maybe Explosion
updateExplosionLifetime dt e =
  let t = expLifetime e - dt
  in if t <= 0 then Nothing else Just e { expLifetime = t }

-- | Calcula las colisiones y aplica sus efectos.
handleCollisions :: GameState -> GameState
handleCollisions gs =
  let objs = map robotBase (robots gs) ++ map projBase (projectiles gs)
  in applyCollisionEffects (checkCollisions objs) gs

-- | Aplica los efectos producidos por todas las colisiones detectadas.
applyCollisionEffects :: [CollisionEvent] -> GameState -> GameState
applyCollisionEffects events gs = foldl applyEffect gs events

-- | Aplica el efecto individual de una colisión (robot vs bala o robot vs robot).
applyEffect :: GameState -> CollisionEvent -> GameState
applyEffect gs (RobotProjectile rid pid) =
  let mr = findById rid (robots gs)
      mp = findProjById pid (projectiles gs)
  in case (mr, mp) of
       (Just r, Just p)
         | projOwner p == objId (robotBase r) -> gs -- Ignora balas del mismo robot
         | otherwise ->
             let damage  = projDamage p
                 hp'     = robotHealth r - damage
                 boom    = Explosion (objPos (projBase p)) 20 0.4
                 projs'  = filter (\q -> objId (projBase q) /= pid) (projectiles gs)
                 robots' = if hp' > 0
                           then replaceRobot (r { robotHealth = hp' }) (robots gs)
                           else filter (\b -> objId (robotBase b) /= rid) (robots gs)
             in gs { robots = robots', projectiles = projs', explosions = boom : explosions gs }
       _ -> gs
applyEffect gs (RobotRobot _ _) = gs

-- ===========================================================
-- 9) RENDER DE PARTIDA
-- ===========================================================
-- Este bloque dibuja todos los elementos visuales de la partida:
-- robots, proyectiles, explosiones y mensajes de fin de juego.
-- ===========================================================

-- | Dibuja el estado actual de la partida.
drawGame :: GameState -> Picture
drawGame gs = pictures $
     map drawRobot (robots gs)
  ++ map drawProjectile (projectiles gs)
  ++ map drawExplosion (explosions gs)
  ++ [drawGameOverMessage gs]

-- | Dibuja el mensaje del ganador o empate al terminar la partida.
drawGameOverMessage :: GameState -> Picture
drawGameOverMessage gs
  | countActiveRobots (robots gs) > 1 = blank
  | otherwise =
      let winnerTxt = case find (\r -> robotHealth r > 0) (robots gs) of
                        Just r  -> "Robot " ++ show (objId (robotBase r)) ++ " WINS!"
                        Nothing -> "IT'S A DRAW!"
      in pictures
          [ translate (-250) 0    $ scale 0.3 0.3 $ color white $ Gloss.text winnerTxt
          , translate (-200) (-50)$ scale 0.2 0.2 $ color white $ Gloss.text "Press R to return to menu"
          ]

-- | Dibuja el cuerpo y torreta de un robot con su barra de vida.
drawRobot :: Robot -> Picture
drawRobot r =
  let base        = robotBase r
      (x,y)       = objPos base
      angleBody   = G.rad2deg (objDir base)
      angleTurret = G.rad2deg (turretDir (robotTurret r))
      c           = getColorForId (objId base)
      body        = color c $ rectangleSolid robotWidth robotHeight
      turret      = pictures [ translate (turretWidth/4) 0 $ color (dark c) $ rectangleSolid turretWidth turretHeight ]
      rotTurret   = rotate (-(angleTurret - angleBody)) turret
  in translate x y $ rotate (-angleBody) $ pictures [body, rotTurret, drawHealthBar r]

-- | Dibuja la barra de salud encima del robot.
drawHealthBar :: Robot -> Picture
drawHealthBar r =
  let ratio   = max 0 (fromIntegral (robotHealth r) / fromIntegral (robotMaxHealth r))
      w       = robotWidth
      yOff    = robotHeight / 2 + 10
      backBar = color red   $ rectangleSolid w 8
      hpBar   = color green $ rectangleSolid (w * ratio) 8
  in translate 0 yOff $ pictures [ backBar, translate (-(w - w * ratio) / 2) 0 hpBar ]

-- | Dibuja un proyectil en movimiento.
drawProjectile :: Projectile -> Picture
drawProjectile p =
  let (x,y) = objPos (projBase p)
      a     = G.rad2deg (objDir (projBase p))
  in translate x y $ rotate (-a) $ color red $ rectangleSolid projectileWidth projectileHeight

-- | Dibuja una explosión circular con desvanecimiento progresivo.
drawExplosion :: Explosion -> Picture
drawExplosion e =
  let (x,y) = expPos e
      alpha = max 0 (expLifetime e / 0.4)
  in translate x y $ color (withAlpha alpha orange) $ circleSolid (expSize e)

-- ===========================================================
-- 10) AUXILIARES
-- ===========================================================
-- Este bloque contiene funciones utilitarias que se usan en
-- diferentes partes del juego para:
--   - Buscar y reemplazar robots o proyectiles
--   - Asignar colores a los robots
--   - Crear formas geométricas
--   - Restringir posiciones dentro de los límites del mapa
-- ===========================================================

-- | Busca un robot por su identificador dentro de una lista.
findById :: Int -> [Robot] -> Maybe Robot
findById i = find (\r -> objId (robotBase r) == i)

-- | Busca un proyectil por su identificador dentro de una lista.
findProjById :: Int -> [Projectile] -> Maybe Projectile
findProjById i = find (\p -> objId (projBase p) == i)

-- | Reemplaza un robot existente por una versión actualizada.
--   Si no existe otro con el mismo ID, simplemente lo agrega.
replaceRobot :: Robot -> [Robot] -> [Robot]
replaceRobot r' =
  (r' :) . filter (\r -> objId (robotBase r) /= objId (robotBase r'))

-- | Asigna un color a cada robot según su ID.
--   Esto permite distinguir visualmente los bots en pantalla.
getColorForId :: Int -> Color
getColorForId 1 = light blue
getColorForId 2 = light red
getColorForId 3 = light green
getColorForId 4 = light yellow
getColorForId _ = white

-- | Crea un polígono rectangular rotado en torno a su centro.
--   Se usa tanto para robots como proyectiles.
createRectanglePolygon :: G.Point -> Float -> Float -> G.Angle -> G.Polygon
createRectanglePolygon (cx, cy) w h ang =
  let hw = w / 2
      hh = h / 2
      -- Vértices del rectángulo sin rotar (centrados en el origen)
      vs = [(-hw, -hh), (hw, -hh), (hw, hh), (-hw, hh)]
      -- Aplica rotación y traslación a cada vértice
      rot = map (rotateAndTranslate (cx, cy) ang) vs
  in G.Polygon rot

-- | Rota un punto alrededor del origen y luego lo traslada al centro indicado.
--   Utilizada internamente por 'createRectanglePolygon'.
rotateAndTranslate :: G.Point -> G.Angle -> G.Point -> G.Point
rotateAndTranslate (cx, cy) ang (px, py) =
  let c = cos ang
      s = sin ang
      x' = px * c - py * s
      y' = px * s + py * c
  in (x' + cx, y' + cy)

-- | Restringe una posición para que no salga del área visible de la pantalla.
--   Evita que los robots se desplacen fuera de los límites definidos.
clampPosition :: G.Point -> G.Point
clampPosition (x, y) =
  let halfW = fromIntegral screenWidth  / 2 - robotWidth  / 2
      halfH = fromIntegral screenHeight / 2 - robotHeight / 2
  in ( max (-halfW) (min halfW x)   -- Limita coordenada X
     , max (-halfH) (min halfH y)   -- Limita coordenada Y
     )

