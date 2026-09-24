SUBDIRS = CODA CTP ENGINE ETRACKING HTRACKING TRACKING UTILS EVDISPLAY PORT
NEWSUBS = ENGINE ETRACKING HTRACKING


all:
	for dir in $(SUBDIRS); do $(MAKE) -C $$dir; done

install:
	for dir in $(SUBDIRS); do $(MAKE) -C $$dir install; done

new:
	for dir in $(NEWSUBS); do $(MAKE) -C $$dir; done

newinstall:
	for dir in $(NEWSUBS); do $(MAKE) -C $$dir install; done

clean:
	for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done


