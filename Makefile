auc_plot:
	Rscript mow.R --operation auc_plot

search_params:
	Rscript mow.R --operation search_params

single_run:
	Rscript mow.R --operation single_run

clean:
	rm -rf build/auc_plot*
	rm -rf build/search_params*
	rm -rf build/single_run*

