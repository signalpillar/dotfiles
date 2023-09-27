-- Text-mode "menu bar indicator" replacement 
canvas = nil 
function createCanvas() 
    if canvas then 
        canvas:hide() 
    end 
    canvas = hs.canvas.new{x = 10, y = 30, h = 200, w = 200}
    canvas[#canvas+1] = {             -- first we start with the entire frame as being available
      action = "build", 
      type = "rectangle",
    }
    canvas[#canvas+1] = {
      type = "rectangle",
      frame = { x = 0, y = 0, h = 22, w = 22},
      fillColor = { alpha = 0.1 },
      action = "fill",
    }
    canvas[#canvas+1] = { 
      type = "text", 
      frame = {x=2, y=0, h="100%", w="100%"}, 
      textFont = "Monaco", 
      textSize = 14, 
      textColor = {hex="#000000"}, 
    } 
    canvas:show()
    canvas:sendToBack() 
    drawInfo() 
end 

function drawInfo() 
  local handle = io.popen("/run/current-system/sw/bin/bb ~/.hammerspoon/stats.clj")
  handle:flush()
  canvas[3].text = handle:read("*all")
  handle:close() 
end 

createCanvas()
 
timer = hs.timer.doEvery(10, drawInfo) 
timer:start()
