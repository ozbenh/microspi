MODEL ?= s25fl128s
all: tb_flash tb_simple

%.o : %.vhdl
	ghdl -a -frelaxed -Pmodel -Pmodel/fmf --std=08 $<

FMF_SRC = $(wildcard model/fmf/*.vhd)
FMF_OBJ = $(patsubst model/fmf/%.vhd, model/fmf/%.o, $(FMF_SRC))

model/%.o : model/%.vhd
	ghdl -a --std=08 -frelaxed -Wshared --workdir=model --work=model -Pmodel/fmf $<

model/fmf/%.o : model/fmf/%.vhd
	ghdl -a --std=08 -frelaxed --workdir=model/fmf --work=fmf $<

fmf: $(FMT_OBJ)
	echo $(FMF_OBJ)

model/$(MODEL).o: $(FMF_OBJ)

spi_simple_ctrl.o: wishbone_types.o spi_rxtx.o
spi_flash_ctrl.o: wishbone_types.o spi_rxtx.o
tb_simple.o: spi_simple_ctrl.o model/$(MODEL).o
tb_flash.o: spi_flash_ctrl.o model/$(MODEL).o

tb_simple: tb_simple.o
	ghdl -e -frelaxed -Pmodel -Pmodel/fmf --std=08 $@
tb_flash: tb_flash.o
	ghdl -e -frelaxed -Pmodel -Pmodel/fmf --std=08 $@

clean:
	rm -f *.o *.cf
	rm -f model/*.o model/*.cf
	rm -f model/fmf/*.o model/fmf/*.cf
	rm -f tb_flash tb_simple
