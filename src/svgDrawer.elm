treatRight newAngle state =
    {state | angle = state.angle + newAngle}
treatLeft newAngle state =
    {state | angle = state.angle - newAngle}
