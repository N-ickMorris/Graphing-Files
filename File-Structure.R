# -----------------------------------------------------------------------------------
# ---- Input Information ------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# choose a work directory
mywd = "C:/Users/Nick Morris/Downloads"
setwd(mywd)

# create a name for a .txt file to log progress information while parallel processing
myfile = "log.txt"

# create the file (this will be in your work directory)
# i suggest opening this up in notepad or notepad++ so you can see how long each task takes when doing parallel processing tasks
file.create(myfile)

# open up a seperate plot window
windows()

}

# -----------------------------------------------------------------------------------
# ---- Packages ---------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# data handling
require(data.table)
require(stringr)
require(tm)

# plotting
require(igraph)

}

# -----------------------------------------------------------------------------------
# ---- Functions --------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# these are functions i like to use

# ---- prints the data types of each column in a data frame -------------------------

types = function(dat)
{
  # make dat into a data.frame
  dat = data.frame(dat)
  
  # get the column names
  column = sapply(1:ncol(dat), function(i) colnames(dat)[i])
  
  # get the class of the columns
  data.type = sapply(1:ncol(dat), function(i) class(dat[,i]))
  
  # compute the number of levels for each column
  levels = sapply(1:ncol(dat), function(i) ifelse(data.type[i] == "factor", length(levels(droplevels(dat[,i]))), 0))
  
  return(data.frame(column, data.type, levels))
}

# ---- a qualitative color scheme ---------------------------------------------------

mycolors = function(n)
{
  require(grDevices)
  return(colorRampPalette(c("#e41a1c", "#0099ff", "#4daf4a", "#984ea3", "#ff7f00", "#ff96ca", "#a65628"))(n))
}

# ---- emulates the default ggplot2 color scheme ------------------------------------

ggcolor = function(n, alpha = 1)
{
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

}

# -----------------------------------------------------------------------------------
# ---- Prepare Data -----------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# import the data
file.structure = data.table(read.csv(file = "File Structure.csv", 
                                     header = FALSE, 
                                     stringsAsFactors = FALSE))

# assign a heirarchy to each entry by counting how many commas are present
file.structure[, Group := str_count(file.structure$V1, ",")]

# remove all punctuation from file.structure output
file.structure[, V1 := removePunctuation(file.structure$V1)]

# go through each observation in file.structure and remove the extra spacing
V1.update = sapply(1:nrow(file.structure), function(i)
{
  # split up each file name by looking for spaces
  V1.split = str_split(file.structure$V1[i], " ")
  
  # find out which entries in V1.split are just blank spaces
  V1.find = which(V1.split[[1]] == "")
  
  # if any blank spaces where found then remove those entries
  if(length(V1.find > 0))
  {
       output = V1.split[[1]][-V1.find]
       
  } else
  {
    output = V1.split[[1]]
  }
  
  # get the name of the folder by combine the remaining entries with 1 space between each entry
  output = paste(output, collapse = " ")
  
  return(output)
})

# finally, append a tag number on each file in file.structure
V1.update = paste0(1:length(V1.update), ": ", V1.update)

# update file.structure with the cleaned file names
file.structure[, V1 := V1.update]

}

# -----------------------------------------------------------------------------------
# ---- Plot Data --------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# create a from-to table indicating the network flow of file.structure
file.tree = lapply(1:nrow(file.structure), function(i)
{
  # if this is a starting node then state it came from the start node
  if(file.structure$Group[i] == min(file.structure$Group))
  {
    # create a from-to node observation
    output = data.table(from = "start node",
                        to = file.structure$V1[i])
    
    return(output)
    
  } else
  {
    # compute which node you came from
    from.node = tail(which(file.structure$Group[1:(i - 1)] == file.structure$Group[i] - 1), 1)
    
    # create a from-to node observation
    output = data.table(from = file.structure$V1[from.node],
                        to = file.structure$V1[i])
    
    return(output)
  }
})

# combine the list of observations into 1 table
file.tree = rbindlist(file.tree)

# extract all of the unique file names in file.structure
nodes = data.table(name = c("start node", unique(file.structure$V1)))

# build a plot of the network flow of file.structure
file.plot = graph.data.frame(file.tree, directed = FALSE, vertices = nodes)

# give nodes an ID column so we can respect the original order of rows
nodes[, ID := 1:nrow(nodes)]

# make V1 the key column in file.structure, and name the key column in nodes
setkey(file.structure, V1)
setkey(nodes, name)

# join file.structure onto nodes
nodes = data.table(file.structure[nodes])

# set any missing node values to 0 (the start node would be the only missing one)
nodes[is.na(Group), Group := 0]

# add 1 to each Group number to shift Group 0 to be Group 1
nodes[, Group := Group + 1]

# compute what the color should be for each GRoup number
node.color = data.table(Group = 1:max(nodes$Group),
                        Color = ggcolor(max(nodes$Group)))

# make Group the key column in node.color and nodes
setkey(node.color, Group)
setkey(nodes, Group)

# join node.color onto nodes
nodes = data.table(node.color[nodes])

# give file.tree an ID column so we can respect the original order of rows
file.tree[, id := 1:nrow(file.tree)]

# make from the key column in file.tree, and V1 the key column in nodes
setkey(file.tree, from)
setkey(nodes, V1)

# join nodes onto file.tree
file.tree = data.table(nodes[file.tree])

# order file.tree by id
file.tree = file.tree[order(id)]

# order nodes by ID
nodes = nodes[order(ID)]

# set up a pdf file to capture the next graphic
pdf("file plot tree.pdf", width = 1400, height = 50, paper = "special") 

# set up graphical parameters
plot.size = .09
edge.width = 10 * plot.size
edge.arrow.width = 0.3 * plot.size
vertex.size = 4 * plot.size
edge.arrow.size = 0.5 * plot.size
vertex.size2 = 3 * plot.size
vertex.label.cex = 15 * plot.size
asp = 0.35 * plot.size
margin = -0.1 * plot.size

# plot the network heirarchy
plot(file.plot, layout = layout.reingold.tilford,
     vertex.color = nodes$Color,
     edge.color = file.tree$Color,
     edge.curved = 0,
     edge.width = edge.width,
     edge.arrow.width = edge.arrow.width,
     vertex.size = vertex.size,
     edge.arrow.size = edge.arrow.size,
     vertex.size2 = vertex.size2,
     vertex.label.cex = vertex.label.cex,
     asp = asp,
     margin = margin)

dev.off()

# export the file tree from-to table
write.table(x = file.tree, 
            file = "File Structure - From To Table.txt", 
            row.names = FALSE, 
            quote = FALSE,
            sep = ",")

}
