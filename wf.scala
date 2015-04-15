
val fileName = Val[String]
val topology = Val[String]
val numAgents = Val[Double]
val connectionProbability = Val[Double]
val initialNeighbours = Val[Double]
val rewiringProbability = Val[Double]
val scaleFreeExponent = Val[Double]
val initialRandomTypes = Val[Double]
val initialMaxi = Val[Double]
val initialMini = Val[Double]
val initialConf = Val[Double]
val strengthOfDilemma = Val[Double]
val inicoop = Val[Double]
val replacement = Val[Double]
val culturalConstant = Val[Double]
val loadtopology = Val[Boolean]
val seed = Val[Int]
val maxi = Val[Double]


val exploration = 
  ExplorationTask(
    (rewiringProbability in (0.0 to 1.0 by 0.20)) x
    (inicoop in (0.0 to 100.0 by 25.0)) x
    (seed in (UniformDistribution[Int]() take 1))
  ) 

val cmds = List(
  "random-seed ${seed}",
  "run-to-grid 10")
  
val basePath = "/iscpif/users/sifuentes/metamimetic/"

val model = 
  NetLogo5Task(basePath + "model/OM_Metamimetic_Networks.nlogo", cmds, true) set (
    inputs += seed,
    fileName := "file",
    topology := "Small-World",
    numAgents := 100.0,
    connectionProbability := 0.0,
    initialNeighbours := 6,
    scaleFreeExponent := 42,
    initialRandomTypes := 1,
    initialMaxi := 0,
    initialMini := 0,
    initialConf := 0,
    strengthOfDilemma := 0.5,
    inicoop := 50.0,
    replacement := 0.0,
    culturalConstant := 5.0,
    loadtopology := false,
    netLogoInputs += (fileName, "file.name"),
    netLogoInputs += (topology, "Topology"),
    netLogoInputs += (numAgents, "Num-Agents"),
    netLogoInputs += (connectionProbability, "Connection-Probability"),
    netLogoInputs += (initialNeighbours, "Initial-Neighbours"),
    netLogoInputs += (rewiringProbability, "Rewiring-Probability"),
    netLogoInputs += (scaleFreeExponent, "Scale-Free-Exponent"),
    netLogoInputs += (initialRandomTypes, "Initial-Random-Types?"),
    netLogoInputs += (initialMaxi, "Initial-Maxi-%"),
    netLogoInputs += (initialMini, "Initial-Mini-%"),
    netLogoInputs += (initialConf, "Initial-Conf-%"),
    netLogoInputs += (strengthOfDilemma, "Strength-of-Dilemma"),
    netLogoInputs += (inicoop, "inicoop"),
    netLogoInputs += (replacement, "replacement?"),
    netLogoInputs += (culturalConstant, "cultural-constant"),
    netLogoInputs += (loadtopology, "Load-Topology?"),    
    netLogoOutputs += ("maxi", maxi)

  )
  
val csvHook = AppendToCSVFileHook(basePath + "output/result.csv")

val env = EGIEnvironment("vo.complex-systems.eu")

val ex = exploration -< (model hook csvHook on env by 5) start
