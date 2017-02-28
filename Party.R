install.packages("party")
library(party)
input1<-readingSkills[c(1:105),]
png(file="decision_tree_png")
output.tree<-ctree(nativeSpeaker~age+shoeSize+score,data = input1,controls = ctree_control(maxdepth = 3))
plot(output.tree)
#dev.off()
#View(readingSkills)
input.dat<-readingSkills[c(1:105),]

#give the chart file a name #
png(file="decision_tree2.png")
    #create a tree#
    
    output.tree<-ctree(nativeSpeaker~age+shoeSize+score,data = input.dat,controls = ctree_control(maxdepth = 3))
    #plot the tree#
    plot(output.tree)
  ================================
    head(datafile)
      png(file="decision_tree_New_png")
    outputtree1<-ctree(Total...Irrigation.potential.created~Irrigation.potential.created...Rabi+Irrigation.potential.created...Kharif,data = datafile,controls = ctree_control(maxdepth = 5))
    plot(outputtree1)
    ---------------------
      View(binary)
    outputTree2<-ctree(admit~gre+gpa+rank,data = binary,controls = ctree_control(maxdepth = 5))
    plot(outputTree2)
    