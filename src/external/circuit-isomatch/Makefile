SRC_DIR = src/
TESTS_DIR = util/lang/

all:
	make -C $(SRC_DIR)

debug:
	make -C $(SRC_DIR) debug

release:
	make -C $(SRC_DIR) release

docs:
	make -C $(SRC_DIR) docs

clean:
	make -C $(SRC_DIR) clean

test: all
	make -C $(TESTS_DIR) test
