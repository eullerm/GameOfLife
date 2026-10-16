import { useState, type ReactNode } from 'react';

interface AccordionProps {
  title: string;
  children: ReactNode;
}

function Accordion({ title, children }: AccordionProps) {
  const [isOpen, setIsOpen] = useState(false);

  return (
    <div className={`rules${isOpen ? ' rules--open' : ''}`}>
      <button
        type="button"
        className="rules__summary"
        aria-expanded={isOpen}
        onClick={() => setIsOpen((open) => !open)}
      >
        {title}
      </button>
      <div className="rules__panel">
        <div className="rules__panel-inner">{children}</div>
      </div>
    </div>
  );
}

export default Accordion;
