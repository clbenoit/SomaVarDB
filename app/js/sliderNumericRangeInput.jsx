const { useState, useEffect, useRef } = React;

const SliderNumericRangeInput = ({ inputId, initialMin, initialMax, initialStep, initialValues }) => {
  const [values, setValues] = useState(initialValues);
  const [min, setMin] = useState(initialMin);
  const [max, setMax] = useState(initialMax);
  const [step, setStep] = useState(initialStep);
  const timeoutRef = useRef(null);

  useEffect(() => {
    window.Shiny.setInputValue(`${inputId}`, values);

    const updateHandler = (newSettings) => {
      if (newSettings.values !== undefined) {
        setValues(newSettings.values);
        window.Shiny.setInputValue(`${inputId}`, newSettings.values);
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

  const handleRangeChange = (e, index) => {
  
    console.log('Range change:', e.target.value, index);
    const newValue = Number(e.target.value);
    const newValues = [...values];
    newValues[index] = newValue;

    if (index === 0 && newValue > newValues[1]) {
      newValues[0] = newValues[1];
    } else if (index === 1 && newValue < newValues[0]) {
      newValues[1] = newValues[0];
    }

    setValues(newValues);
    //window.Shiny.setInputValue(`${inputId}`, newValues);
  };
  
  const handleRangeMouseUp = (e, index) => {
  
    console.log('send range to shiny:', e.target.value, index);
    const newValue = Number(e.target.value);
    const newValues = [...values];
    newValues[index] = newValue;

    if (index === 0 && newValue > newValues[1]) {
      newValues[0] = newValues[1];
    } else if (index === 1 && newValue < newValues[0]) {
      newValues[1] = newValues[0];
    }
    window.Shiny.setInputValue(`${inputId}`, newValues);
  };  

  const handleNumberChange = (e, index) => {
  
    console.log('Number change:', e.target.value, index);
    const newValue = Number(e.target.value);
    const newValues = [...values];
    newValues[index] = newValue;

    if (index === 0 && newValue > newValues[1]) {
      newValues[0] = newValues[1];
    } else if (index === 1 && newValue < newValues[0]) {
      newValues[1] = newValues[0];
    }

    setValues(newValues);

    if (timeoutRef.current) {
      clearTimeout(timeoutRef.current);
    }

    timeoutRef.current = setTimeout(() => {
      if (!isNaN(newValue) && newValue >= min && newValue <= max) {
        window.Shiny.setInputValue(`${inputId}`, newValues);
      }
    }, 400);
  };

  const fillStyle = {
    background: `linear-gradient(to right, #ddd ${((values[0] - min) / (max - min)) * 100}%, rgb(233, 84, 32) ${((values[0] - min) / (max - min)) * 100}%, rgb(233, 84, 32) ${((values[1] - min) / (max - min)) * 100}%, #ddd ${((values[1] - min) / (max - min)) * 100}%)`
  };

  return (
    <div className="slider-container">
      <div className="slider-wrapperRange">
        <div className="slider-value" style={{ left: `${((values[0] - min) / (max - min)) * 75}%` }}>{values[0]}</div>
        <div className="slider-value" style={{ left: `${((values[1] - min) / (max - min)) * 75}%` }}>{values[1]}</div>
        <input
          type="range"
          id={`${inputId}-lower`}
          min={min}
          max={max}
          step={step}
          value={values[0]}
          onChange={(e) => handleRangeChange(e, 0)}
          onMouseUp={(e) => handleRangeMouseUp(e, 0)}
          className="sliderRange"
          style={{ background: 'transparent', zIndex: 5 }}
        />
        <input
          type="range"
          id={`${inputId}-upper`}
          min={min}
          max={max}
          step={step}
          value={values[1]}
          onChange={(e) => handleRangeChange(e, 1)}
          onMouseUp={(e) => handleRangeMouseUp(e, 1)}
          className="sliderRange"
          style={{ ...fillStyle, zIndex: 1 }}
        />
        <div className="slider-labelsRange">
          <span className="min-label">{min}</span>
          <span className="max-label">{max}</span>
        </div>
      </div>
      <div className="number-inputs-container">
        <input
          id={`${inputId}-number-lower`}
          type="number"
          step={step}
          value={values[0]}
          onChange={(e) => handleNumberChange(e, 0)}
          className="number-inputRange"
        />
        <input
          id={`${inputId}-number-upper`}
          type="number"
          step={step}
          value={values[1]}
          onChange={(e) => handleNumberChange(e, 1)}
          className="number-inputRange"
        />
      </div>
    </div>
  );
};

export default SliderNumericRangeInput;
