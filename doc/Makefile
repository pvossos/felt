TEX=	felt.tex misc/title.tex misc/foreword.tex misc/appendix.tex \
	    misc/abib.tex text/intro.tex text/analysis.tex text/problem.tex \
        text/elements.tex text/felt_prog.tex text/winfelt.tex \
        text/xfelt.tex text/velvet_tex text/velvet_solve.tex \
        text/corduroy.tex text/algorithm.tex text/adding_elts.tex

FIGURES= figures/velvet_analysis.epsi figures/velvet_element.epsi \
         figures/velvet_node.epsi figures/velvet_animate.epsi \
         figures/velvet_file.epsi figures/velvet_output.epsi \
         figures/velvet_ascii.epsi figures/velvet_force.epsi \
         figures/velvet_stresses.epsi figures/velvet_colors.epsi \
         figures/velvet_graph.epsi figures/velvet_structure.epsi \
         figures/velvet_configure.epsi figures/velvet_load.epsi \
         figures/velvet_tools.epsi figures/velvet_constraint.epsi \
         figures/velvet_main.epsi figures/velvet_trimesh.epsi \
         figures/velvet_contour.epsi figures/velvet_material.epsi \
         figures/velvet_wireframe.epsi

all:	dvi	

clean:  cleantex cleanps

dvi:	felt.dvi

ps:	felt.ps

felt.dvi:$(TEX) $(FIGURES)
	latex felt.tex || touch felt.tex
	-@grep -s Rerun felt.log && touch felt.tex

felt.ps:	felt.dvi
		dvips felt.dvi -o felt.ps

cleantex:
	rm -f *.aux */*.aux *.log *.lof *.toc felt.dvi felt.dvi-err 

cleanps:
	rm -f felt.ps
