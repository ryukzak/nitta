-- This is a classic example of the system dynamic model. The model description
-- presented here:
-- <https://pysd-cookbook.readthedocs.io/en/latest/analyses/getting_started/Hello_World_Teacup.html>.
-- Calculations are performed in fixed-point numbers. Decimals are not
-- converted.

function teacup(time, temp_cup)
    local temp_ch =   10
    local temp_room = 70
    local time_step = 0.125

    send(time)
    send(temp_cup)

    time = time + time_step
    local acc = temp_room - temp_cup
    local temp_loss, _ = acc / temp_ch

    local delta = temp_loss * time_step
    temp_cup = temp_cup + delta

    teacup(time, temp_cup)
end
teacup(0, 180)
