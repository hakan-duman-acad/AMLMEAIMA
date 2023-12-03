require(reticulate)

source_python("pyFiles/_dalex.py")

vi_plot = DxVI()
vi_plot$prepare_all()

vi_gr_plot = DxVIgr()
vi_gr_plot$prepare_all()

ale_plots = DxAle(all=TRUE)
ale_plots$prepare_all()
