complete_expenses <- function(dataset) {
  
  dataset |>
    mutate(TotalExpenses = RoomService + FoodCourt + ShoppingMall + Spa + VRDeck,
           NoExpenses = (TotalExpenses == 0),
           LogTotalExpenses = if_else(TotalExpenses == 0, - 1, log(TotalExpenses)),
           LogRoomService = if_else(RoomService == 0, - 1, log(RoomService)),
           LogFoodCourt = if_else(FoodCourt == 0, - 1, log(FoodCourt)),
           LogShoppingMall = if_else(ShoppingMall == 0, - 1, log(ShoppingMall)),
           LogSpa = if_else(Spa == 0, - 1, log(Spa)),
           LogVRDeck = if_else(VRDeck == 0, - 1, log(VRDeck)),
           NoRoomService = (RoomService == 0),
           NoFoodCourt = (FoodCourt == 0),
           NoShoppingMall = (ShoppingMall == 0),
           NoSpa = (Spa == 0),
           NoVRDeck = (VRDeck == 0))
}

check_association <- function(dataset, var, group_var,group_size_var) {
  
  table((dataset |>
           filter(!is.na({{var}}) & !is.na({{group_var}}) & {{group_size_var}} > 1) |>
           group_by({{group_var}}) |>
           summarize(n = length(unique({{var}}))))$n)
}

retraitement_donnees <- function(dataset) {
  
  dataset |>
    mutate(IdGroup = word(PassengerId, 1, sep = fixed("_")) |> as.integer(),
           IdNumber = word(PassengerId, 2, sep = fixed("_")) |> as.integer(),
           CabinArea = word(Cabin, 1, sep = fixed("/")) |> factor(),
           CabinNumber = word(Cabin, 2, sep = fixed("/")) |> as.integer(),
           CabinSide = word(Cabin, 3, sep = fixed("/")) |> factor(),
           FirstName = word(Name, 1) |> factor(),
           LastName = word(Name, 2) |> factor()) |> 
    group_by(IdGroup) |> 
    mutate(GroupSize = max(IdNumber)) |> 
    ungroup() |> 
    add_count(Cabin, name = "CabinSize") |>
    mutate(CabinSize = if_else(is.na(Cabin), NA_integer_, CabinSize)) |> 
    group_by(CabinArea, CabinSide) |> 
    mutate(SectorCabinCount = max(CabinNumber, na.rm = TRUE) + 1,
           SectorPersonCount = n()) |> 
    ungroup() |> 
    mutate(CabinPosition = CabinNumber / (SectorCabinCount - 1),
           HomePlanet = factor(HomePlanet),
           Destination = factor(Destination)) |> 
    complete_expenses()
}

na_replace <- function(v) {
  
  out <- v |> unique()
  if (all(is.na(out))) out else na.omit(v)[[1]]
}
  
imputation_certaine <- function(dataset) {

  dataset |>
    mutate(HomePlanet = case_when(
      is.na(HomePlanet) & !is.na(CabinArea) & CabinArea %in% c("A", "B", "C") ~ "Europa",
      is.na(HomePlanet) & !is.na(CabinArea) & CabinArea == "G" ~ "Earth",
      TRUE ~ HomePlanet) |> factor(levels = levels(HomePlanet)),
      VIP = if_else(!is.na(HomePlanet) & HomePlanet == "Earth", FALSE, VIP),
      CryoSleep = if_else(!is.na(NoExpenses) & !NoExpenses, FALSE, CryoSleep)) |> 
    mutate(across(RoomService:VRDeck, \(x) if_else(
      (!is.na(Age) & Age < 13) | (!is.na(CryoSleep) & CryoSleep), 0, x))) |> 
    group_by(IdGroup) |> 
    mutate(HomePlanet = na_replace(HomePlanet),
           CabinSide = na_replace(CabinSide)) |> 
    ungroup() |> 
    complete_expenses()
}

most_frequent <- function(v) {
  
  (tibble(x = v) |> count(x))$x[[1]]
}

imputation_par_defaut <- function(dataset) {
  
  dataset |> 
    group_by(HomePlanet, CabinArea) |> 
    mutate(across(where(is.numeric), \(x) {x[is.na(x)] <- median(x, na.rm = TRUE); x})) |> 
    mutate(across((where(is.logical) | where(is.factor)) & !Transported, \(x) {x[is.na(x)] <- most_frequent(x); x})) |>
    group_by(HomePlanet) |> 
    mutate(across(where(is.numeric), \(x) {x[is.na(x)] <- median(x, na.rm = TRUE); x})) |> 
    mutate(across((where(is.logical) | where(is.factor)) & !Transported, \(x) {x[is.na(x)] <- most_frequent(x); x})) |>
    group_by(CabinArea) |> 
    mutate(HomePlanet = {HomePlanet[is.na(HomePlanet)] <- most_frequent(HomePlanet); HomePlanet}) |>
    ungroup() |> 
    complete_expenses()
}

submit_model <- function(model_name, dataset) {
  
  model <- get(model_name)
  predictions <- predict(model, dataset, type = "response")
  submission <-
    tibble(PassengerId = dataset$PassengerId,
           Transported = (predictions >= 0.5) |> if_else("True", "False") |> factor())
  write_csv(submission, file = paste0("submissions/", model_name,".csv"))
  
  print(paste0("Model ", model_name, " correctly submitted"))
  return(submission)
}
