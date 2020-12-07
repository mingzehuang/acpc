pageWithSidebar(
  headerPanel('Area Classification by Robust Sparse PCA Kmeans Clustering for Two Principle Components'),
  sidebarPanel(
    numericInput('clusters', 'Cluster count', 3, min = 1, max = 9),
    hr(),
    numericInput('pc', 'Which Principle Component', 2, min = 1, max = 2)),
  mainPanel(
    plotOutput('plot1'),
    hr(),
    plotOutput('plot2')
  )
)