#create myGGTheme
myGGTheme <- ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5, face = "bold.italic", size=16), #plot title aesthetics
                   plot.subtitle = ggplot2::element_text(hjust = 0.5, face = "bold", size = 12), #plot subtitle aesthetics
                   axis.title.x = ggplot2::element_text(size = 12, color= "black", face = "bold"), #x axis title aesthetics
                   axis.title.y = ggplot2::element_text(size = 12, color= "black", face = "bold"), #y axis title aesthetics
                   axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 12),
                   #legend aesthetics
                   legend.title = ggplot2::element_text(size= 14,
                                    color = "black",
                                    face = "bold"),
                   legend.title.align = 0.5,
                   legend.text = ggplot2::element_text(size = 10,
                                              color = "black",
                                              face = "bold"),
                   legend.text.align = 0)

#' Get Q Values for Plotting
#'
#' This function gets the Q values so we can plot them over time. It filters the
#' relevant Q values for the image supplied, transmutes it so there's only the
#' name column and the second state Q value. It then groups by name and adds a
#' row (trial) number, pivoting the data wider so there is one column for each
#' of Q values (one for each possible image path). It then calculates the mean
#' of those two Q values and puts that in a column with the same name as the
#' image input. For instance, the Q value of AR is the mean of the Q values for
#' RAR and LAR.
#'
#' @param Qtable The Qtable from a simulation (i.e. the output of
#'   \code{\link{generateData}})
#' @param image The image ID to be selected (either "AR", "AL", "BR", or "BL").
#'
#' @importFrom rlang .data
#' @return A dataframe with 201 rows and 4 columns that have the row number
#'   (trial number), Q values for both of the paths leading to an image and the
#'   average of those Q values.
#' @export
#'

getQGraphicValues <- function(Qtable, image) {

  #take the Qtable, convert it into a tibble, and add a name column according to the associated Q values.
  Qtibble <- Qtable %>% tidyr::tibble() %>%
    dplyr::mutate(name = base::rep(c("RAR", "RAL", "RBR", "RBL", "LAR", "LAL", "LBR", "LBL"), base::nrow(Qtable)/8)) %>%
    dplyr::select(name, dplyr::everything())

  output <- Qtibble %>%
    dplyr::filter(stringr::str_detect(.data$name, {{ image }})) %>%
    dplyr::transmute(name = name,
                     image = .[,2]) %>%
    dplyr::group_by(name) %>%
    #the row_number is necessary to help dissociate the identical values in each
    #constituent column when pivoting.
    dplyr::mutate(row = dplyr::row_number()) %>%
    tidyr::pivot_wider(names_from = name, values_from = image) %>%
    dplyr::mutate("{{ image }}" := base::rowMeans(dplyr::select(., tidyselect::contains({{ image }}))))

  return(output)
}

#' Get Basic Visualizations for Simulations
#'
#' @param data Simulation data -- the output of \code{\link{generateData}}.
#'
#' @return A series of plots that can be indexed into via the \code{$} operator.
#'   Specifically, the following plots can be accessed: \itemize{ \item
#'   \emph{$Prop1Plot:} A bar plot with the proportions of actions (right or
#'   left) in State FC. \item \emph{$Prop2Plot:} A bar plot with the proportions
#'   of actions (right or left) in States A and B (faceted appropriately). \item
#'   \emph{$State1ActPlot:} A line plot of actions (right or left) in State FC
#'   over time. \item \emph{$State2ActPlot:} A line plot of actions (right or
#'   left) in States A and B (faceted appropriately) over time. \item
#'   \emph{$ImagePropQPlot:} A line plot showing the probability of receiving a
#'   reward and the average Q value associated with each image over time. \item
#'   \emph{$Qtable:} A data frame containing the final Q table for this
#'   simulation. \item \emph{$QRorL:} A data frame that has the Q values
#'   associated with going right or left from the first state. }
#' @export
#'

basicVisualization <- function(data) {

  #getFinalQtable and set it as dataframe
  QtableOut <- utils::tail(data$Qtable,8)
  base::rownames(QtableOut) <- c("RAR", "RAL", "RBR", "RBL", "LAR", "LAL", "LBR", "LBL")

  #get data frame of QL and QR
  QRorL <- base::data.frame("QLeft from FC" = dplyr::last(data$QL), "QRight from FC" = dplyr::last(data$QR))

  #create Qtable values -- must use original data since graphsData has removed Qtable
  ARvalues <- getQGraphicValues(data$Qtable, "AR")
  ALvalues <- getQGraphicValues(data$Qtable, "AL")
  BRvalues <- getQGraphicValues(data$Qtable, "BR")
  BLvalues <- getQGraphicValues(data$Qtable, "BL")

  #select the means from each value and add type = Q-value, row = row_number.
  plotQData <- dplyr::bind_cols(dplyr::select(ARvalues, `"AR"`),
                                dplyr::select(ALvalues, `"AL"`),
                                dplyr::select(BRvalues, `"BR"`),
                                dplyr::select(BLvalues, `"BL"`)) %>%
    dplyr::mutate(type = "Q-Value", row = dplyr::row_number())

  graphsData <- manipulateData(data)

  #create an imageProbAndQplot that shows the probability
  #over time of reward at each image and average Q value
  ImageProbAndQPlot <- graphsData %>%
    dplyr::select(probAR, probAL, probBR, probBL) %>%
    dplyr::rename(AR = probAR, AL = probAL, BR = probBR, BL = probBL) %>%
    dplyr::mutate(type = "Probability", row = dplyr::row_number()) %>%
    dplyr::bind_rows(plotQData) %>%
    tidyr::pivot_longer(., cols = -c(type, row), names_to = "Image", values_to = "Value") %>%
    ggplot2::ggplot(aes(x = row, y = Value, color = Image)) +
    ggplot2::geom_line(aes(linetype = type)) +
    ggplot2::labs(color = "Image", y = "Value", x = "Trials", linetype = "Value Type") +
    ggplot2::scale_color_manual(values = c("deepskyblue1","darkorchid2", "springgreen1", "orange")) +
    ggplot2::ggtitle("Probabilities of Reward and Average Q-values \n for Each Image Over Time") +
    ggplot2::facet_wrap(~type) +
    myGGTheme

  #get bar plot with proportion of actions in state1
  #data is used instead of graphsData since we want
  #the factor as opposted to numeric coloring.
  #We thus divide n by 8 to get appropriate counts.
  act1PropPlot <- graphsData %>%
    dplyr::count(action1) %>%
    mutate(action1 = case_when(action1 == 0 ~ "Left",
                               action1 == 1 ~ "Right",
                               TRUE ~ "Other")) %>%
    ggplot2::ggplot(aes(action1, n, fill = action1)) +
    ggplot2::geom_col() +
    ggplot2::scale_x_discrete(labels = c("Left", "Right")) +
    ggplot2::scale_fill_manual(values = c("red", "blue"), labels = c("Left", "Right")) +
    ggplot2::labs(fill = "Action 1", y = "Count", x = "Action") +
    ggplot2::ggtitle("Actions from State 1") +
    myGGTheme

  #get bar plot with proportion of actions in state2
  #data is used instead of graphsData since we want
  #the factor as opposted to numeric coloring.
  #We thus divide n by 8 to get appropriate counts.
  act2PropPlot <- graphsData %>%
    dplyr::mutate(action2 = dplyr::case_when(action2 == 0 ~ "Left",
                               action2 == 1 ~ "Right",
                               TRUE ~ "Other"),
           state2 = dplyr::case_when(str_detect(.$state2, "A") == TRUE ~ "A",
                              TRUE ~ "B")) %>%
    dplyr::count(action2, state2) %>%
    ggplot2::ggplot(aes(action2, n, fill = action2)) +
    ggplot2::geom_col() +
    ggplot2::scale_x_discrete(labels = c("Left", "Right")) +
    ggplot2::scale_fill_manual(values = c("red", "blue"), labels = c("Left", "Right")) +
    ggplot2::labs(fill = "Action 2", y = "Count", x = "Action") +
    ggplot2::ggtitle("Actions from State 2") +
    ggplot2::facet_wrap(~state2) +
    myGGTheme

  #get proportion of actions over time from state 1
  State1Actions <- ggplot2::ggplot(graphsData, aes(x = 1:base::nrow(graphsData))) +
    ggplot2::geom_line(aes(y = propA1R, color = "blue")) +
    ggplot2::geom_line(aes(y = propA1L, color = "red")) +
    ggplot2::labs(color = "Action", y = "Proportion", x = "Trials") +
    ggplot2::scale_color_discrete(labels = c("Right", "Left")) +
    ggplot2::ggtitle("Actions from State 1 over time") +
    myGGTheme

  #get proportion of actions over time from state 2
  State2Actions <- graphsData %>%
    dplyr::mutate(state2 = dplyr::case_when(str_detect(.$state2, "A") == TRUE ~ "A",
                              TRUE ~ "B")) %>%
    ggplot2::ggplot(aes(x = 1:base::nrow(graphsData))) +
    ggplot2::geom_line(aes(y = propA2R, color = "blue")) +
    ggplot2::geom_line(aes(y = propA2L, color = "red")) +
    ggplot2::labs(color = "Action", y = "Proportion", x = "Trials") +
    ggplot2::scale_color_discrete(labels = c("Right", "Left")) +
    ggplot2::ggtitle("Actions from State 2 over time") +
    ggplot2::facet_wrap(~state2) +
    myGGTheme

  return(list("Prop1Plot" = act1PropPlot,
              "Prop2Plot" = act2PropPlot,
              "State1ActPlot" = State1Actions,
              "State2ActPlot" = State2Actions,
              "ImageProbQPlot" = ImageProbAndQPlot,
              "Qtable" = QtableOut,
              "QRorL" = QRorL))

}

# stayProbPlot Function ---------------------------------------------------
# stayProbPlot function graphs the stay probabilities as a result of performing
# a logistic regression. It checks whether the data is premodeled. If it is, it
# just plots them. If not, it calls the functions logSetup, getLogFit,
# getLogPreds, and then plots the stay proportions based on the logistic
# regression model -- predicting on the data from which it was fit. Indicate
# whether the dataType is MF or MB for plot title purposes. The preprocessed
# simulated data with no table should be passed in, or the output of a
# non-simulated dataset. If alpha = TRUE (by default) the plot will be faceted
# by the alpha term.

#' Plot the Stay Probability
#'
#' This function graphs an agent's probability of choosing the same action in
#' State FC based on the prior trial's transition type (common or rare) as well
#' as whether the last trial was rewarded. This "stay probability" is calculated
#' according to a logistic regression model, predicting on the data from which
#' it was fit. This function checks whether the data has been pre-modeled,
#' meaning the logistic regression probabilities have already been calculated.
#' If so, this function just plots the probabilities. If not, this function
#' calls \code{logSetup}, \code{getLogFit}, \code{getLogPreds}, and then plots
#' the data.
#'
#' Please indicate whether the data was "MF" or "MB" (see parameter dataType)
#' for plot title purposes. The plot can be faceted by learning rates and
#' temporal discount terms (see parameters alpha and gamma).
#'
#' @param data The output of \code{\link{generateData}}. Data can be passed
#'   through \code{\link{manipulateData}} first for model-free simulations,
#'   though that is unnecessary. However, simulated data should be processed.
#'   Data can also be passed through \code{getLogPreds}, though this is also
#'   unnecessary (see parameter preModeled).
#' @param preModeled Logical: TRUE if the data has been passed through
#'   \code{getLogPreds}; FALSE otherwise.
#' @param dataType Should be a character string. Either "MF" if the simulation
#'   was purely model-free (x = 0) or "MB" if the simulation had model-based
#'   influence (x > 0).
#' @param x If the parameter dataType is "MB", please enter the x used in
#'   generating simulation data. This will add more information to the plot,
#'   though it is unnecessary.
#' @param alpha Logical: TRUE means the plot will be faceted by the learning
#'   rate alpha; false otherwise. Default is TRUE.
#' @param gamma Logical: TRUE means the plot will be faceted by the temporal
#'   discount rate gamma; false otherwise. Default is TRUE.
#'
#' @return A plot showing the stay probabilities for a given simulation/agent.
#' @export
#'

stayProbPlot <- function(data, preModeled, dataType, x = NULL, alpha = TRUE, gamma = TRUE) {

  if (!preModeled) {

    # check if the data has been processed (like the correct DynaQ data or an already amnipulated Q-learning dataframe).
    # If already manipulated, it would have the column "transition" and we would see it in the str_detect, so this would not run.
    # If it hasn't been manipulated, call manipulate data.
    if( base::sum(stringr::str_detect(base::names(data), "transition")) == 0) {data <- manipulateData(data)}

    modelData <- logSetup(data) #setup the data for logistic regression
    modelPreds <- getLogPreds(modelData) #get the model predictions bound to the original data
    modelPreds <- modelPreds %>% dplyr::add_count(reward, transition, predStay)
  }else if (preModeled) {
    modelPreds <- data
  }

  #create appropriate subtitle based on dataType (and optionally x)
  if (dataType == "MF") {
    subtitle <- "Q-Learning"
  }else if (dataType == "MB") {
    if (base::is.null(x)) {
      subtitle <- "DynaQ"
    }else if(!base::is.null(x)){
      subtitle <- base::paste("DynaQ, x = ", x)
    }
  }else {
    subtitle <- dataType
  }

  #plot the predicted stay trends
  plot <- modelPreds %>%
    dplyr::mutate(reward = base::as.factor(reward),
           reward = forcats::fct_recode(reward, No = "-1", Yes = "1"),
           transition = base::as.factor(transition),
           transition = forcats::fct_recode(transition, Rare = "-1", Common = "1")) %>%
    ggplot2::ggplot(aes(x = reward, y = predStay, fill = transition)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::ggtitle("Stay Probability Trends",
            subtitle = subtitle) +
    ggplot2::labs(y = "Stay Probability", x = "Reward", fill = "Transition Type") +
    myGGTheme

  if (alpha & gamma) {
    plot <- plot +
      ggplot2::facet_grid(ggplot2::vars(alpha), ggplot2::vars(gamma), labeller = ggplot2::label_both) +
      ggplot2::labs(caption = "Faceted by the learning rate, alpha, and temporal discount rate, gamma.")

  }else if (alpha & !gamma) {
    plot <- plot +
      ggplot2::facet_wrap(~alpha) +
      ggplot2::labs(caption = "Faceted by the learning rate, alpha.")

  }else if (gamma & !alpha) {
    plot <- plot +
      ggplot2::facet_wrap(~gamma) +
      ggplot2::labs(caption = "Faceted by the temporal discount rate, gamma.")
  }

  return(plot)

}




# simTransitionPlot Function ----------------------------------------------
# By saving the trackTrans dataframe, we can plot the model's estimate of the
# transition structure over time. If text and hline are true, annotations
# will be added to the graph (they are by default.)

#' Plot the Transition Model's Estimates Over Time
#'
#' @param trackedTransData The \code{trans$Track} for a given simulation.
#' @param text Logical: TRUE will display text on the plot describing the true
#'   transition probabilities. FALSE will disable this. Default is TRUE.
#' @param hline Logical: TRUE will display a horizontal line on the plot at the
#'   true transition probabilities. FALSE will disable this. Default is TRUE.
#'
#' @return A plot showing the model's estimates of transition probabilities over time.
#' @export
#'

simTransitionPlot <- function(trackedTransData, text = TRUE, hline = TRUE) {

  #create a dataframe that has the true probabilities of transitioning to each image for displaying on the graph
  #in text
  plotText <- dplyr::tribble(~label,                                            ~action,  ~x,    ~y,   ~statePrime,
                             "True probability of left leading to A is 70%",     "Left", 1500,  0.72,      "A",
                             "True probability of left leading to B is 30%",     "Left", 1500,  0.32,      "B",
                             "True probability of right leading to A is 30%",  "Right", 1500,  0.32,      "A",
                             "True probability of right leading to B is 70%",  "Right", 1500,  0.72,      "B",
  )
  #create a dataframe that has the y value for the true probability for plotting a geom_hline
  plotHLine <- dplyr::tribble(~action,  ~y, ~statePrime,
                       "Left",  0.7,     "A",
                       "Left",  0.3,     "B",
                       "Right", 0.3,     "A",
                       "Right", 0.7,     "B",
  )

  #generate a plot of the etimated transition probabilities over time
  plot <- trackedTransData %>%
    #select the probabilities
    dplyr::select(propRB:propLA) %>%
    #rename them
    dplyr::rename(RB = propRB,
           RA = propRA,
           LB = propLB,
           LA = propLA) %>%
    #add a row number
    dplyr::mutate(row = row_number()) %>%
    #pivot the data so each image has a probability at each row (where row is trial)
    tidyr::pivot_longer(cols = -row, names_to = "statePrime", values_to = "probability") %>%
    #add an action column where if the image has an R (i.e. RA or RB), action is Right,
    #else it's Left. Also replace the statePrime with "A" or "B" based on whether it's
    #RA/RB or LA/LB.
    dplyr::mutate(action = dplyr::case_when(str_detect(statePrime, "R") ~ "Right",
                              TRUE ~ "Left"),
           statePrime = dplyr::case_when(str_detect(statePrime, "A") ~ "A",
                                  TRUE ~ "B")) %>%
    #plot the row (trial) and probability, with the image as color.
    ggplot2::ggplot(aes(x = row, y = probability, color = statePrime)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Estimated Transition Structure",
         subtitle = "Based on First Action in DynaQ Simulations",
         x = "Trial (Real and Simulated)",
         y = "Probability",
         color = "Next State") +
    #make y axis in percent
    ggplot2::scale_y_continuous(labels = scales::percent, n.breaks = 10) +
    #facet_wrap based on action associated with each image
    ggplot2::facet_wrap(~action) +
    myGGTheme

  #if text = TRUE, add the true probability text
  if(text) {
    plot <- plot +
      ggplot2::geom_text(data = plotText,
                mapping = aes(x = x,
                              y = y,
                              label = label),
                show.legend = FALSE)
  }

  #if hline = TRUE, add the hline.
  if(hline) {
    plot <- plot +
      ggplot2::geom_hline(data = plotHLine,
                          mapping = aes(yintercept = y,
                                        color = statePrime),
                          linetype = "dashed")
  }

  return(plot)

}

#' Plot the Distribution of Stay Probabilities for Multiple Agents
#'
#' This function plots a histogram of the distribution of stay probabilities for
#' each agent. The plot is faceted by transition type (common or rare) and
#' reward (yes or no).
#'
#' @param data Data is a single dataframe that has had the
#'   \code{\link{manipulateData}} function applied to it. Should be a dataframe
#'   with multiple agents.
#'
#' @return A histogram showing the distribution of stay probabilities amongst
#'   multiple agents.
#' @export
#'

stayDistributionPlot <- function(data) {
  data %>%
    dplyr::distinct(reward, transition, predStay) %>%
    dplyr::arrange(reward, transition) %>%
    dplyr::mutate(reward = base::as.factor(reward),
           reward = forcats::fct_recode(reward, No = "-1", Yes = "1"),
           transition = base::as.factor(transition),
           transition = forcats::fct_recode(transition, Rare = "-1", Common = "1")) %>%
    ggplot2::ggplot(aes(x = predStay, fill = reward)) +
    ggplot2::geom_histogram(bins = 20, show.legend = FALSE) +
    ggplot2::scale_x_continuous(breaks = seq(0,1, by = 0.05)) +
    ggplot2::facet_grid(ggplot2::vars(reward), ggplot2::vars(transition),
               switch = "y") +
    ggplot2::labs(x = "Probability of Staying", y = "Count",
         title = "Distribution of Stay Probabilities",
         subtitle = "By transition type and reward cues") +
    myGGTheme +
    ggplot2::theme(#panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill="lightblue"),
      strip.text = element_text(face = "bold", size = 10))
}


#' Create a Summary Table of the Stay Probabilities
#'
#' This function creates a summary table of the mean, standard deviation,
#' minimum, and maximum stay probabilities based on the predictions from a
#' logistic regression. It is specific to each reward and transition structure
#' combination.
#'
#' @param data Data is a single dataframe that has had the
#'   \code{\link{manipulateData}} function applied to it. Should be a dataframe
#'   with multiple agents.
#'
#'   alpha gamma reward transition meanPredStay stdPredStay minPredStay
#'   maxPredStay
#'
#' @return A dataframe with 8 coiumns and variable rows depending on the
#'   combinations of the learning rate, alpha, and temporal discount factor,
#'   gamma:
#'
#'   \itemize{ \item \emph{alpha:} Numeric -- learning rate, alpha. \item
#'   \emph{gamma:} Numeric -- temporal discount factor, gamma. \item
#'   \emph{reward:} Factor -- yes or no. \item \emph{transition:} Factor -- common
#'   or rare transition type. \item \emph{meanPredStay:} Numeric -- the mean
#'   predicted probability of staying. \item \emph{stdPredStay:} Numeric -- the
#'   standard deviation of the predicted probability of staying. \item
#'   \emph{minPredStay:} Numeric -- the minimum predicted probability of staying.
#'   \item \emph{maxPredStay:} Numeric -- the maximum predicted probability of
#'   staying. }
#'
#' @export
#'

summaryTable <- function(data) {
  data %>% group_by(alpha, gamma, reward, transition) %>%
    summarize(meanPredStay = mean(predStay),
              stdPredStay = sd(predStay),
              minPredStay = min(predStay),
              maxPredStay = max(predStay),
              .groups = 'drop') %>%
    mutate(reward = as.factor(reward),
           reward = fct_recode(reward, No = "-1", Yes = "1"),
           transition = as.factor(transition),
           transition = fct_recode(transition, Rare = "-1", Common = "1"))
}


