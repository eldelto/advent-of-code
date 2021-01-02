package main

import (
	"fmt"
	"os"

	"github.com/gdamore/tcell/v2"
)

func initTcell() tcell.Screen {
	tcell.SetEncodingFallback(tcell.EncodingFallbackASCII)
	s, err := tcell.NewScreen()
	if err != nil {
		fmt.Fprintf(os.Stderr, "%v\n", err)
		os.Exit(1)
	}
	if err = s.Init(); err != nil {
		fmt.Fprintf(os.Stderr, "%v\n", err)
		os.Exit(1)
	}

	s.SetStyle(tcell.StyleDefault.
		Foreground(tcell.ColorWhite).
		Background(tcell.ColorBlack))
	s.Clear()

	return s
}

func drawTiles(s tcell.Screen, tiles []*Tile) {
	s.Clear()
	for _, tile := range tiles {
		drawTile(s, tile)
	}
	s.Show()
}

func drawTile(s tcell.Screen, tile *Tile) {
	width := len(tile.Data) + 1
	height := width + 1

	writeString(s, fmt.Sprintf("Tile: %d", tile.ID), width*tile.Position.X, height*tile.Position.Y)
	for y, row := range tile.Data {
		for x, r := range row {
			s.SetContent(width*tile.Position.X+x, height*tile.Position.Y+y+1, r, nil, tcell.StyleDefault)
		}
	}
}

func writeString(s tcell.Screen, content string, x, y int) {
	for i, r := range content {
		s.SetContent(x+i, y, r, nil, tcell.StyleDefault)
	}
}

func waitForEsc(s tcell.Screen, do func(tcell.Screen, <-chan struct{})) {
	quit := make(chan struct{})
	go func() {
		for {
			ev := s.PollEvent()
			switch ev := ev.(type) {
			case *tcell.EventKey:
				switch ev.Key() {
				case tcell.KeyEscape, tcell.KeyEnter:
					close(quit)
					return
				case tcell.KeyCtrlL:
					s.Sync()
				}
			case *tcell.EventResize:
				s.Sync()
			}
		}
	}()

	do(s, quit)

	select {
	case <-quit:
	}
}
