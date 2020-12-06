pageWithSidebar(
  headerPanel('Area Classification by Robust Sparse PCA Kmeans clustering'),
  sidebarPanel(
    numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)