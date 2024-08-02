const { useState, useEffect, useRef } = React;

const SliderNumericInput = ({ inputId, initialMin, initialMax, initialStep, initialValue, fillDirection = 'left' }) => {
  const [value, setValue] = useState(initialValue);
  const [min, setMin] = useState(initialMin);
  const [max, setMax] = useState(initialMax);
  const [step, setStep] = useState(initialStep);
  const timeoutRef = useRef(null); // Use useRef to keep track of the timeout ID

  useEffect(() => {
    window.Shiny.setInputValue(`${inputId}`, value);

    const updateHandler = (newSettings) => {
      if (newSettings.value !== undefined) {
        setValue(newSettings.value);
        window.Shiny.setInputValue(`${inputId}`, newSettings.value);
      }
      if (newSettings.min !== undefined) {
        setMin(newSettings.min);
      }
      if (newSettings.max !== undefined) {
        setMax(newSettings.max);
      }
      if (newSettings.step !== undefined) {
        setStep(newSettings.step);
      }
    };

    window.Shiny.addCustomMessageHandler(`${inputId}`, updateHandler);

    return () => {
      window.Shiny.removeCustomMessageHandler(`${inputId}`, updateHandler);
    };
  }, [inputId]);

  const handleRangeChange = (e) => {
    setValue(Number(e.target.value));
  };

  const handleRangeMouseUp = (e) => {
    window.Shiny.setInputValue(`${inputId}`, e.target.value);
  };

  const handleNumberChange = (e) => {
    const newValue = e.target.value;
    setValue(newValue);

    if (timeoutRef.current) {
      clearTimeout(timeoutRef.current); // Clear the previous timeout if it exists
    }

    timeoutRef.current = setTimeout(() => {
      if (newValue !== '' && !isNaN(newValue) && newValue >= min && newValue <= max) {
        window.Shiny.setInputValue(`${inputId}`, newValue);
      }
    }, 400);
  };

  const fillStyle = fillDirection === 'left'
    ? { background: `linear-gradient(to right, rgb(233, 84, 32) ${(value - min) / (max - min) * 100}%, #ddd ${(value - min) / (max - min) * 100}%)` }
    : { background: `linear-gradient(to right, #ddd ${(value - min) / (max - min) * 100}%, rgb(233, 84, 32) ${(value - min) / (max - min) * 100}%)` };

  return (
    <div className="slider-container">
      <div className="slider-wrapper">
        <div className="slider-value" style={{ left: `${((value - min) / (max - min)) * 75}%` }}>{value}</div>
        <input
          type="range"
          id={inputId}
          min={min}
          max={max}
          step={step}
          value={value}
          onChange={handleRangeChange}
          onMouseUp={handleRangeMouseUp}
          className="slider"
          style={fillStyle}
        />
        <div className="slider-labels">
          <span className="min-label">{min}</span>
          <span className="max-label">{max}</span>
        </div>
      </div>
      <input
        type="number"
        step={step}
        value={value}
        onChange={handleNumberChange}
        className="number-input"
      />
    </div>
  );
};

export default SliderNumericInput;
